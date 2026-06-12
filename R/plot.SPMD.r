
## Visualizes control times and times under exposure visually, given
## a fitted SPMD object
#' @importFrom data.table :=
#' @importFrom data.table setnames
#' @importFrom data.table setkey
#' @importFrom data.table setorder
#' @importFrom data.table shift
#' @export
plot.SPMD <- function(x, show_events=FALSE, fill_control="steelblue",
                      fill_exposed="red", event_size=1, event_shape=8,
                      event_color="black", xlab="Time", ...) {

  . <- .stop <- .time <- .A <- .start <- .id <- .new_id <-
    .grp <- .GRP <- NULL

  stopifnotm(inherits(x, "SPMD"), "'x' must be an SPMD object, created",
             " using the sym_pair_matching() function.")

  # get relevant matched data
  d_times <- unique(x$d_matches, by=c(".id", ".time"))
  d_times[, .stop := .time + x$inputs$risk_period]
  setnames(d_times, old=".time", new=".start")

  # get unique exposure times
  d_exp <- unique(
    d_times[.A==TRUE], by=c(".id", ".start")
  )[, c(".id", ".start", ".stop")]

  # create new ids, sorted by first exposure time
  setkey(d_exp, .start, .id)
  d_exp[, .new_id := .GRP, by=.id]

  # extract unique observation periods
  setorder(d_times, .id, .start)
  d_times[, .grp := cumsum(.start > shift(cummax(.stop), fill=-Inf)), by=.id]
  d_times <- d_times[, .(
    .start = min(.start),
    .stop  = max(.stop)
  ), by = .(.id, .grp)]

  # merge new id to it
  d_times <- merge(d_times, d_exp[, c(".id", ".new_id")], by=".id", all.x=TRUE,
                   allow.cartesian=TRUE)

  if (show_events) {
    d_events <- copy(x$d_events)
    d_events <- merge(d_events, d_exp[, c(".id", ".new_id")], by=".id",
                      all.x=FALSE, all.y=FALSE)
  }

  # plot it
  p <- ggplot2::ggplot(NULL) +
    ggplot2::geom_rect(data=d_times,
                       ggplot2::aes(xmin=.start,
                                    xmax=.stop,
                                    ymin=.new_id - 0.5,
                                    ymax=.new_id + 0.5),
                       fill=fill_control, linewidth=0) +
    ggplot2::geom_rect(data=d_exp,
                       ggplot2::aes(xmin=.start,
                                    xmax=.stop,
                                    ymin=.new_id - 0.5,
                                    ymax=.new_id + 0.5),
                       fill=fill_exposed, linewidth=0) +
    ggplot2::labs(x=xlab) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y=ggplot2::element_blank(),
                   axis.title.y=ggplot2::element_blank(),
                   axis.ticks.y=ggplot2::element_blank(),
                   panel.border=ggplot2::element_blank())

  if (show_events) {
    p <- p + ggplot2::geom_point(data=d_events,
                                 ggplot2::aes(x=.time, y=.new_id),
                                 size=event_size, shape=event_shape,
                                 color=event_color)
  }

  return(p)
}
