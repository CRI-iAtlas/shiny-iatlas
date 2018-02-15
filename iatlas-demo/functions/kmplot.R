#survivalmodule
create_kmplot <- function(fit, df, confint, risktable, variable){
    ggsurvplot(
        fit, 
        data = df, 
        conf.int = confint, 
        risk.table = risktable, 
        title = getNiceName(variable))
}

