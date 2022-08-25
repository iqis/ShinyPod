#' Construct a Module
#'
#' @param fn module definition; <function>
#' @param ... static data shared by all instances of this type; <dot-dot-dot>
#'
#' @field .our shared static data; <environment>
#' @field .my the module itself; <environment>
#'
#' @field bare_id ID of the module instance, verbatim as supplied with; <character>

#' @field parent_module module in which this module is called; <ShinyPod:instance>
#' @field parent_id parent module's ID; <character>
#' @field parent_ns parent module's Shiny namespace; <function>

#' @field id ID of this instance , namespaced by parent `ns`; <character>
#' @field ns Shiny namespace of this module, itself namespaced by parent `ns`; <function>
#'
#' @field empty ui; <ShinyPod:ui_frag|function>
#' @field emtpy server; <ShinyPod:server_frag|function>
#'
#' @field state exposed state of the module; <reactivevalues>
#' @field run run the module a Shiny app; <function>
#'
#' @return <ShinyPod:type>
#' @export
#'
#' @examples
Module <- function(fn, ...){

  static_env <- list2env(list(...))

  type_context <- parent.frame()
  type_raw_args <- formals(fn)
  type_raw_body <- body(fn)

  type_args <- c(alist(bare_id =),
                 type_raw_args,
                 alist(expr = NULL))

  type_body <- quote({
    inst_context <- parent.frame()
    inst_expr <- substitute(expr)

    inst_identity <- new.env()
    with(inst_identity, {
      bare_id <- bare_id

      parent_module <- inst_context
      parent_id <- get0("id",
                        envir = parent_module,
                        inherits = FALSE)
      parent_ns <- shiny::NS(parent_id)

      id <- parent_ns(bare_id)
      ns <- shiny::NS(id)

      state <- reactiveValues()

      run <- function(...){
        stopifnot(!exists("ui", envir = .my))
        stopifnot(!exists("server", envir = .my))

        shinyApp(fluidPage(ui()),
                 function(input, output, session){
                   server()
                 },
                 ...)
      }
    })

    inst_env <- new.env(parent = inst_context)

    type_arg_env <- new.env(parent = type_context)
    type_env <- new.env(parent = type_arg_env)
    type_env$.my <- inst_env
    type_env$.our <- static_env

    copy_all_bindings(inst_identity, type_env)
    environment(type_env$run) <- type_env

    lapply(names(type_raw_args),
           function(x){
             # delayedAssign(x,
             # get(x),
             # assign.env = type_arg_env,
             # eval.env = parent.frame(1))
             assign(x,
                    get(x),
                    envir = type_arg_env)
           })

    eval(type_raw_body, type_env)

    copy_all_bindings(type_env, inst_env)
    eval(inst_expr, inst_env)

    structure(inst_env,
              class = c("ShinyPod:instance"))
  }) # end type_body

  structure(eval(call("function",
                      as.pairlist(type_args),
                      type_body)),
            class = "ShinyPod:type")
}

#' @rdname Module
#' @export
Pod <- Module

#' Contruct a Shiny UI/Server Fragment
#' @name ui_server
#'
#' @param fn Fragment definition; <function>
#'
#' @return <ShinyPod:ui_frag>/ShinyPod:server_frag>
#' @export
#'
#' @examples
UI <- function(fn){
  .shim <- new.env(parent = environment(fn))
  .shim$ns <- get0("ns", envir = parent.frame(), inherits = FALSE)
  .shim$id <- get0("id", envir = parent.frame(), inherits = FALSE)
  environment(fn) <- .shim
  structure(fn,
            class = "ShinyPod:ui_frag")
}

#' @rdname ui_server
#' @export
Server <- function(fn){
  .shim <- new.env(parent = parent.frame())
  .shim$.shim <- .shim

  .shim$bare_id <- get0("bare_id",
                        parent.frame(),
                        inherits = FALSE,
                        ifnotfound = NULL)

  .shim$inner_fn_args <- alist(input = ,
                               output = ,
                               session =)
  .shim$inner_fn_body <- body(fn)

  .shim$outer_fn_args <- formals(fn)
  .shim$outer_fn_body <- quote({
    moduleServer(.shim$bare_id,
                 eval(call("function",
                           as.pairlist(.shim$inner_fn_args),
                           .shim$inner_fn_body)))
  })

  structure(eval(call("function",
                      as.pairlist(.shim$outer_fn_args),
                      .shim$outer_fn_body),
                 envir = .shim),
            class = "ShinyPod:server_frag")
}
