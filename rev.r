##' Simple Othello
##' - G.K.
CONST.TIME.LIMIT <- 6  # threshold of time(sec) for searching.
CONST.MAX.DEPTH <- 3  # max depth of depth-first searching.
##' main
othello <- function(verbose = FALSE) {
    ## initialize.
    board <- as.numeric(rep(0,8^2))
    turn <- 1
    board[conv.xy2index(4,4)] <- board[conv.xy2index(5,5)] <- 1
    board[conv.xy2index(4,5)] <- board[conv.xy2index(5,4)] <- -1

    ## determine player types.
    turn.type <- c()
    for (i in c(3,1)) {
        cat(ifelse(i == 3,"1st","2nd"),"player(",ifelse(i == 3,"WHITE","BLACK"),") is ")
        turn.type[i] <- c("Human","Computer")[menu(c("Human","Computer"))]
    }
    
    while (TRUE) {
        if ( length(get.index.available(turn, board)) == 0 ) {
            if ( length(get.index.available(-turn, board)) == 0)
                break  # game over, no one can put disc.
            turn <- turn * -1
            next  # pass
        }
        if ( turn.type[turn + 2] == "Human")
            board <- set.disc(display.board.plot(board, input=TRUE, turn=turn), turn=turn, board=board)  # input by player
        if ( turn.type[turn + 2] == "Computer") {
            display.board.plot(board,turn=turn)
            index <- search.tree(depth = 0, depth.max=CONST.MAX.DEPTH, turn, board, verbose)  # computer search the next.
            board <- set.disc(index, turn=turn, board=board)
        }
        turn <- turn * -1
    }
    display.board.plot(board,turn=turn)
}
##' evaluating board as score
eval.board <- function(board) {
    score <- c( 8,-2, 3, 3, 3, 3,-2, 8,
               -2, 1, 2, 2, 2, 2, 1,-2,
                3, 2, 1, 1, 1, 1, 2, 3,
                3, 2, 1, 1, 1, 1, 2, 3,
                3, 2, 1, 1, 1, 1, 2, 3,
                3, 2, 1, 1, 1, 1, 2, 3,
               -2, 1, 2, 2, 2, 2, 1,-2,
                8,-2, 3, 3, 3, 3,-2, 8)
    return (sum(score * board) + sum(board))
}
##' depth-first search, root must be depth = 0.
search.tree <- function(depth = 0, depth.max, turn, board, verbose=FALSE) {

    ## for verbose
    outputlog <- function(str="") {
        cat(paste(paste(rep(".",depth),collapse=""),"f(",index,")=",minmax$score, str, collapse=""),"\n")
    }

    if (depth == 0) {
        search.tree.time <<- proc.time()  # for checking condition on interruption.
        search.tree.terminate <<- FALSE  # for interruption event.
    }
    if (depth == depth.max) {
        if (CONST.TIME.LIMIT <= (proc.time() - search.tree.time)[3])
            search.tree.terminate <<- TRUE
        return (eval.board(board))
    }
    minmax <- c()
    minmax$score <- -1e9 * turn
    minmax$index <- NA
    for (index in get.index.available(turn, board)) {
        board.tmp <- set.disc(index=index, turn=turn, board=board)
        ## depth first search by recursive function.
        score <- search.tree(depth = depth + 1, depth.max = depth.max, turn= -1 * turn, board=board.tmp, verbose=verbose)

        ## Min Max Algorithm
        ## - for white turn
        if (turn == 1 && minmax$score <= score) {
            minmax$score <- score
            minmax$index <- index
            if (verbose)
                outputlog()
        }
        ## - for black turn
        if (turn == -1 && score <= minmax$score ) {
            minmax$score <- score
            minmax$index <- index
            if (verbose)
                outputlog()
        }
        ## Check exceeding the threshold time(sec) in searching.
        if (search.tree.terminate) {
            minmax$score <- eval.board(board)
            if (verbose)
                outputlog("* Termination")
            break
        }
    }
    ## root on searching
    if (depth == 0)
        return (minmax$index)
    else
        return (minmax$score)
}
##' Display board (Or input)
##' (input = TRUE) : input by user.
display.board.plot <- function(board, input = FALSE, turn = NA) {
    image(matrix(rep(c(seq(8) %% 2,(seq(8) + 1) %% 2), 4), nrow=8) ,col=c("green","darkgreen"), axes=FALSE)
    title(paste(ifelse(turn== 1,"[White]"," White ")," = ",sum(board==1),"  vs  ",
                ifelse(turn==-1,"[Black]"," Black ")," = ",sum(board==-1),sep=""))
    grid(8,8,lty=1,col="green")
    for (i.turn in c(1,-1)) {  # plot disc
        place <- sapply(which(board == i.turn), conv.index2xy)
        points(as.integer(place / 10 - 1) * 1/7, (place %% 10 - 1) * 1/7,
               cex=5/7*min(dev.size()), col=ifelse(i.turn == 1,"white","black"),pch=16)
    }
    if (!is.na(turn)) {  # plot availability
        place.xy <- sapply(place.index <- get.index.available(turn, board), conv.index2xy)
        if (0 < length(place.index))
            points(as.integer(place.xy / 10 - 1) * 1/7, (place.xy %% 10 - 1) * 1/7,
                   cex=5/7*min(dev.size()), col=ifelse(turn == 1,"white","black") ,pch=1,lty=4)
    }
    while (input) {  # input using locator()
        input.location <- locator(1)
        if ((input.place <- conv.xy2index(min(which(input.location$x < seq(1/7/2,1+1/7/2,by=1/7) )),
                                          min(which(input.location$y < seq(1/7/2,1+1/7/2,by=1/7) )))) %in% place.index) {
            return ( input.place )
        }
    }
}
##' convert b/w coordinate and index.
conv.xy2index <- function(x, y) {
    (y-1) * 8 + x
}
conv.index2xy <- function(index) {
    tmp <- index %% 8
    ifelse(tmp == 0, 8, tmp) * 10 + ifelse(tmp == 0, as.integer(index / 8), as.integer(index / 8) + 1)
}
##' get all index available.
get.index.available <- function(turn, board) {
    which(sapply(seq(1,8),
                  function(y) { sapply(seq(1,8),
                                       function(x) {
                                           set.disc(conv.xy2index(x, y) ,turn=turn, board=board, check=TRUE)
                                       })}))
}
##' (check=FALSE) set turn's disc on the specified.
##' (check=TRUE)  check to set turn disc on the specified index.
set.disc <- function(index=NA, turn, board, check = FALSE) {
    if (check && board[index] != 0)
        return(FALSE)
    if (!check)
        board.new <- board
    for (x in seq(-1,1)) {
        for (y in seq(-1,1)) {
            if (x == 0 && y == 0)
                next
            x.new <- as.integer(conv.index2xy(index) / 10) + cumsum(rep(x,7))
            y.new <- conv.index2xy(index) %% 10 + cumsum(rep(y,7))
            index.new <- 1 <= x.new & x.new <= 8 & 1 <= y.new & y.new <= 8  # boundary validation.
            move.list <- unlist(mapply(conv.xy2index, x.new[index.new], y.new[index.new]))
            tmp <- unique(board[move.list])
            if (2 <= length(tmp) && tmp[1] == -turn && tmp[2] == turn) {
                if (check) return (TRUE)  # possible to put.
                board.new[ c(index, move.list[seq(1, min(which(board[move.list] == turn))-1)]) ] <- turn  # reverse discs.
            }
        }
    }
    if (check)
        return (FALSE)  # impossible to put.
    return (board.new)
}
## play
## othello()
