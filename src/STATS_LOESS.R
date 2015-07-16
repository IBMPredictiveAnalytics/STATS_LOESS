"STATS LOESS extension command"

#Licensed Materials - Property of IBM
#IBM SPSS Products: Statistics General
#(c) Copyright IBM Corp. 2014
#US Government Users Restricted Rights - Use, duplication or disclosure 
#restricted by GSA ADP Schedule Contract with IBM Corp.


helptext = 'STATS LOESS DEPENDENT=dependent variable
[INDEPENDENT=independent variables]
[INTERACTIONS=integer
[/OPTIONS MISSING={OMIT* | FAIL} ALPHA=number DEGREE=polynomialdegree
NORMALIZE = {YES * | NO} FAMILY={GAUSSIAN* | SYMMETRIC}
SURFACE={INTERPOLATE*|DIRECT} STATISTICS={APPROXIMATE*| EXACT}]
/PREDICTIONS DATASET=datasetname
[X1SEQ = number number number ... X4SEQ = number number number
X1LIST = number ... number X4NUMBER = number ... number
STDERRORS = {NO* | YES}]
[/MODELFILE SAVEMODEL=filespec LOADMODELMODEL=filespec]
[/HELP].

This command fits a loess regression model.  It fits a low order polynomial to
local subsets of the data with the weight of the points diminishing as the distance
from the center point increases.

This command can be used in two ways.  The first fits a model, creates a prediction
dataset, and optionally saves the fitted model.  The second uses a previously 
saved model to produce a new prediction dataset.  For the second case, the 
keywords for specifying the model and the entire OPTIONS subcommand are
omitted, and the MODELFILE subcommand is used to reference a previously 
saved model.

DEPENDENT and INDEPENDENT name the variables in the model.  There must
be one to four independent variables.  Variables must all be scale-level numeric.

INTERACTIONS indicates what interaction terms are included in the model.
A value of 1 means main effects only; 2 means all two-way interactions and so on.
The default value is 1.

MISSING indicates whether cases with missing values are ignored or whether
they cause the procedure to stop.  Predictions will have SYSMIS values
where inputs are missing.

ALPHA indicates the amount of smoothing used for the loess fit, e.g., alpha=.5
causes a span of 50% of the data to be used.  Alpha can exceed 1, which 
uses all the cases at each point but slows the decay of the weights.

DEGREE is the degree of the polynomials that are fitted.  It can be 0, 1, or 2.
A value of 0 means local constant fitting, but the R documentation says to use
this setting "with caution".

NORMALIZE = YES causes the predictors to be normalized to a common scale
if there is more than one.

FAMILY determines whether fitting is by least squares (Gaussian) or by
"a re-descending M estimator with the Tukey biweight function".

SURFACE indicates whether to compute the surface exactly or via interpolation.

STATISTICS indicates whether to compute approximate statistics or exact ones.
Exact computation is time consuming.

The MODELFILE subcommand allows saving models and using them later.
SAVEMODEL takes a file specification for a model.  It is conventional to 
use the extension .Rdata for this.

LOADMODEL specifies a file containing a previously saved model.  When
used, other model specifications should not be given, and X values must
be given on the PREDICTION subcommand.  If X values are not given.
the output will still display the properties of the loaded model.

The PREDICTION subcommand determines what predicted values are
created.  DATASET specifies the name of the dataset.  The name must not
already be in use.

If there are no X specifications, the predicted values for the estimation data
and, optionally, their standard errors are saved in the dataset along with 
the dependent and independent variables.  

Alternatively, the X specifications can indicate a different set of points 
where predictions should be calculated.  X specifications must be
used when referring to a saved model in order to produce predictions.
The active dataset is ignored in that case.

These rules.apply to the X specifications.
- There must be as many X specifications as there are independent
variables in the model with X1 corresponding to the first independent variable
and so on.
- Each X specification can be either a list of values (LIST) or a sequence
(SEQ).  The sequence specification is a triple of starting value, ending value,
and increment.  For example, X1SEQ=10 30 10 is equivalent to the values
10, 20, 30.  If the third number is omitted, it is assumed to be 1.
Various other shortcuts are allowed.
- If the X specifications are not all the same length, the shorter ones are
recycled to generate the additional values needed.
- The prediction dataset includes the X values used, the predictions, and,
optionally, the standard errors.

/HELP displays this text and does nothing else.
'

# author=  'jkp, IBM'
# version=  '1.0.1'
# history
#12-03-2012 original version

# override for api to account for extra parameter in V19 and beyond
StartProcedure <- function(procname, omsid) {
    if (substr(spsspkg.GetSPSSVersion(),1, 2) >= 19) {
       spsspkg.StartProcedure(procname, omsid)
    }
    else {
       spsspkg.StartProcedure(omsid)
    }
}


doloess <- function(dep=NULL, indep=NULL, datasetname=NULL, interactions=1,
		missingvalues="omit", alpha=.50, degree=2, normalize=TRUE, family="gaussian", 
		surface="interpolate", statistics="approximate",
		x1seq=NULL, x2seq=NULL, x3seq=NULL, x4seq=NULL, 
		x1list=NULL, x2list=NULL, x3list=NULL, x4list=NULL, stderrors=FALSE,
		savemodel=NULL, loadmodel=NULL) {

		# enable localization		
		domain <- "STATS_LOESS"
    if (Sys.info()[[1]]=="Darwin"){
       majorVersion<-strsplit(spsspkg.GetSPSSVersion(),".",fixed=TRUE)[[1]][[1]]
       if (as.integer(majorVersion) >= 19)
          basepath<-file.path("/Library/Application Support/IBM/SPSS/Statistics",majorVersion)
       else
           basepath<-file.path("/Library/Application Support/SPSSInc/PASWStatistics",majorVersion)
    } else {basepath<-file.path(spsspkg.GetStatisticsPath())}
    res<-bindtextdomain(domain,dirname=file.path(basepath,"extensions",domain,"lang"))
    paths<-strsplit(Sys.getenv("SPSS_EXTENSIONS_PATH"),.Platform$path.sep)[[1]]
    if (!identical(paths,character(0))){
        for (i in 1:length(paths)){
            dirname<-file.path(paths[[i]],domain,"lang")
            if (file.exists(dirname)){
                res<-bindtextdomain(domain,dirname=dirname)
                break
            }
        }
    }
    
    if ("*" %in% spssdata.GetDataSetList()) {
				stop(gtxt("The active dataset must have a name, because otherwise it would be closed by this procedure"))
		}
		if ((is.null(dep) || is.null(indep)) && is.null(loadmodel)) {
				stop(gtxt("Dependent and independent variables must be specified unless loading a saved model"))
		}
		if (!is.null(savemodel) && !is.null(loadmodel)) {
				stop(gtxt("Cannot save and load model at the same time"))
		}
		if (!is.null(loadmodel) && (!is.null(dep) || !is.null(indep))) {
				stop(gtxt("Cannot specify model variables if loading a saved model"))
		}
		if (is.null(loadmodel)) {  # normal estimation routine
				indlen = length(indep)
				if (indlen > 4) {
						stop(gtxt("A maximum of four independent variables can be specified"))
				}
				missingcodes = list("omit"=na.omit, "fail"=na.fail)
				allvars = c(dep, indep)
				model = paste(indep, collapse="+")
				if (interactions > 1) {
						model = sprintf("(%s)^%s", model, interactions)
				}
				model = paste(dep, model, sep="~")
				dta <- spssdata.GetDataFromSPSS(allvars, missingValueToNA = TRUE, factorMode = "labels")
				for (v in colnames(dta)) {
						if (is.factor(dta[,v])) {
								stop(sprintf(gtxt("All variables must be scale level numeric: %s"), v))
						}
				}
				# The loess call can fail if there are too few points to estimate the model for a given point
				res = loess(model, data=dta, na.action=missingcodes[missingvalues][[1]], span=alpha, degree=degree,
						normalize=normalize, family=family, control=loess.control(surface=surface, statistics=statistics))
				ressum= summary(res)
		
				# pivot table fit summary
				fitlbls = c(gtxt("Output Dataset"), gtxt("Alpha"), gtxt("Polynomial Degree"), gtxt("Interactions Level"),
						gtxt("Family"), gtxt("Normalize"), gtxt("Surface"), gtxt("Cell"), gtxt("Missing Values"), gtxt("Statistics Computation"),
						gtxt("Number of Observations"), gtxt("Equivalent Number of Parameters"), gtxt("Residual Standard Error"),
						gtxt("Model to Be Saved As"))
						
				fitsum = list(datasetname,ressum$pars$span, ressum$pars$degree, interactions,
						ressum$pars$family, ifelse(ressum$pars$normalize, gtxt("Yes"), gtxt("No")), ressum$pars$surface,
						ressum$pars$cell, missingvalues, statistics,
						ressum$n, sprintf("%.3f", ressum$enp), sprintf("%.5f", ressum$s),
						ifelse(is.null(savemodel), gtxt("Not saved"), savemodel))
				names(fitsum) = fitlbls
				fitsum = data.frame(cbind(fitsum))
				colnames(fitsum) = gtxt("Values")
				if (!is.null(savemodel)) {
						save(res, fitsum, dep, indep, indlen, file=savemodel)  # allvars excluded
				}
		} else {  # restore previously saved model
				load(loadmodel)
				fslength = nrow(fitsum)
				fi = file.info(loadmodel)[["mtime"]]
				fitsum[fslength,1] = paste(loadmodel, fi, sep="\n")
				row.names(fitsum)[fslength] = gtxt("Model Source")
				allvars = indep
		}
		
		StartProcedure(gtxt("LOESS Fit"), "STATSLOESS") 
		spsspivottable.Display(fitsum, 
				title = sprintf(gtxt("Settings and Statistics for Variable %s"), dep),
				templateName = "LOESSFIT",
				outline=gtxt("Summary"),
				caption = paste(sprintf(gtxt("Predictors: %s"),paste(indep, collapse=" ")), gtxt("Calculated by R function loess"), sep="\n")
		)
		spsspkg.EndProcedure()
		# fits - if new x's were specified, insert those values
		# up to number of variables used in loess and drop the dependent variable
		poffset = 1  # assume no new x values
		for (x in c(x1list, x1seq,x2list,x2seq,x3list,x3seq,x4list,x4seq)) {
				if (!is.null(x)) {  # using new predictors
						poffset = 0
						dta = builddf(indep, x1list, x1seq, x2list, x2seq, x3list, x3seq, x4list, x4seq)
						break
				}
		}
		if (!is.null(loadmodel) && poffset == 1) {
				stop(gtxt("New x values must be specified if using a model loaded from a file"))
		}
		# missing values will generate an NA value rather than omitting rows				
		predvalues = predict(res, dta, se = stderrors)
		# make sure assigned names are unique
		name = "predicted"
		xynames = tolower(allvars)
		suffix = 1
		while (name %in% xynames) {
				name = paste(name, suffix, sep="")
				suffix = suffix + 1
		}
		# append predicted values column and optionally se's at end
		# Note: the structure returned b predict is completely different when se's are computed
		# from the case when they are not :-(
		if (!stderrors) {
				dta[, indlen+1+poffset] = predvalues
		} else {
				dta[, indlen+1+poffset] = predvalues["fit"]
		}
		colnames(dta)[indlen+1 + poffset] = name
		if (stderrors) {
				suffix = 1
				name = "predictedse"
				while (name %in% xynames) {
							name = paste(name, suffix, sep="")
							suffix = suffix + 1
				}
				dta[,indlen+2 + poffset] = predvalues["se.fit"]
				colnames(dta)[indlen+2+poffset] = name
		}
		
		# build a new SPSS dataset
		if (is.null(loadmodel)) {  # not used loaded model
				inputdict = spssdictionary.GetDictionaryFromSPSS(allvars)
				if (poffset == 0)
						inputdict = inputdict[-1]   # remove dep variable
				}
		else { # loaded model - ignore active dataset
				inputdict = data.frame(row.names=c("varName", "varLabel", "varType", "varFormat", "varMeasurementLevel"))
				for (v in indep) {
						inputdict = data.frame(inputdict, c(v, "", 0, "F8.2", "scale"))
				}
		}
		datadict = data.frame(inputdict, c("predicted", gtxt("Predicted Values"), 0, "F8.2", "scale"))
		if (stderrors) {
				datadict = data.frame(datadict, c("predictedse", gtxt("Standard Errors"), 0, "F8.2", "scale"))
		}
		tryCatch({
				spssdictionary.SetDictionaryToSPSS(datasetname, datadict)
				spssdata.SetDataToSPSS(datasetname, dta)}, 
						error=function(e) {print(e)
                cat(gtxt("Failed to create prediction dataset. Dataset name must not already exist: "), datasetname)
                }
		)
		spssdictionary.EndDataStep()
		
		# clean up workspace
    tryCatch(rm(list=ls()), warning = function(e) {return(NULL)})
}

builddf = function(indep, x1list, x1seq, x2list, x2seq, x3list, x3seq, x4list, x4seq) {
		# build a data frame from up to 4 predictors either from a list of values or a sequence specification
		# assume at least one x.
		# x's need to be consecutive up to the number of independent variables
		
		x1 = buildx(x1seq, x1list)
		x2 = buildx(x2seq, x2list)
		x3 = buildx(x3seq, x3list)
		x4 = buildx(x4seq, x4list)

		# check for enough x specifications with no holes
		i = 0
		for (v in list(x1,x2,x3,x4)) {
				i = i + 1
				if (is.null(v)) {
						stop(gtxt("As many consecutive x specifications must be supplied as there are variables in the model"))
				}
				if (i >= length(indep)) {
						break
				}
		}

		maxlength = do.call(max, lapply(list(x1,x2,x3,x4), length))  # length of longest x column
		# repeat elements of each predictor as necessary so that they are all of the same maximum length
		# unused x's will just pad to a single NULL
		x1 = rep(x1, length.out=maxlength)
		x2 = rep(x2, length.out=maxlength)		
		x3 = rep(x3, length.out=maxlength)		
		x4 = rep(x4, length.out=maxlength)
		df = data.frame(x1)
		for (x in list(x2, x3, x4)) {
				if (!is.null(x)) {
						df = data.frame(df, x)
				}
		}
		colnames(df) = indep
		return(df)
}

buildx = function(xseq, xlist) {
		# generate a sequence either from a list of values or a seq specification
		# 0 or 1 argument can be non-NULL

		if (!is.null(xseq) && !is.null(xlist)) {
				stop(gtxt("Cannot specify both a sequence and a list form for the same x variable"))
		}
		x = NULL
		if (!is.null(xlist)) {
				x = xlist
		}
		if (!is.null(xseq)) {
				x = tryCatch(do.call(seq, as.list(xseq)), 
				error=function(e) {
						stop(sprintf(gtxt("Invalid x sequence specification: %s"), paste(xseq, sep=" ", collapse=" ")))
				}
				)
		}
		return(x)
}


gtxt <- function(...) {
		return(gettext(...,domain="STATS_LOESS"))
	}

gtxtf <- function(...) {
		return(gettextf(...,domain="STATS_LOESS"))
	}
	
Run <- function(args) {
    #Execute the STATS LOESS extension command

    cmdname = args[[1]]
    args = args[[2]]
    oobj = spsspkg.Syntax(list(
        spsspkg.Template("DEPENDENT", subc="",  ktype="existingvarlist", var="dep", islist=FALSE),
        spsspkg.Template("INDEPENDENT", subc="", ktype="existingvarlist", var="indep", islist=TRUE),
        spsspkg.Template("INTERACTIONS", subc="",  ktype="int", var="interactions"),
				spsspkg.Template("MISSING", subc="OPTIONS", ktype="str", var="missingvalues", vallist=list("omit","fail")),
				spsspkg.Template("ALPHA", subc="OPTIONS", ktype="float", var="alpha", vallist=list(0)),
				spsspkg.Template("DEGREE", subc="OPTIONS", ktype="int", var="degree", vallist=list(0, 2)),
        spsspkg.Template("NORMALIZE", subc="OPTIONS", ktype="bool", var="normalize"),
        spsspkg.Template("FAMILY", subc="OPTIONS", ktype="str", var="family", vallist=list("gaussian","symmetric")),
				spsspkg.Template("SURFACE", subc="OPTIONS", ktype="str", var="surface", vallist=list("interpolate","direct")),
				spsspkg.Template("STATISTICS", subc="OPTIONS", ktype="str", var="statistics", vallist=list("approximate","exact")),
        spsspkg.Template("DATASET", subc="PREDICTIONS", ktype="varname", var="datasetname"),
        spsspkg.Template("X1SEQ", subc="PREDICTIONS", ktype="float", var="x1seq", islist=TRUE),
				spsspkg.Template("X2SEQ", subc="PREDICTIONS", ktype="float", var="x2seq", islist=TRUE),
				spsspkg.Template("X3SEQ", subc="PREDICTIONS", ktype="float", var="x3seq", islist=TRUE),
				spsspkg.Template("X4SEQ", subc="PREDICTIONS", ktype="float", var="x4seq", islist=TRUE),
        spsspkg.Template("X1LIST", subc="PREDICTIONS", ktype="float", var="x1list", islist=TRUE),
				spsspkg.Template("X2LIST", subc="PREDICTIONS", ktype="float", var="x2list", islist=TRUE),
				spsspkg.Template("X3LIST", subc="PREDICTIONS", ktype="float", var="x3list", islist=TRUE),
				spsspkg.Template("X4LIST", subc="PREDICTIONS", ktype="float", var="x4list", islist=TRUE),
        spsspkg.Template("STDERRORS", subc="PREDICTIONS", ktype="bool", var="stderrors"),
				spsspkg.Template("SAVEMODEL", subc="MODELFILE", ktype="literal", var="savemodel"),
				spsspkg.Template("LOADMODEL", subc="MODELFILE", ktype="literal", var="loadmodel"),
        spsspkg.Template("HELP", subc="", ktype="bool")
				))
        

    # A HELP subcommand overrides all else
    if ("HELP" %in% attr(args,"names")) {
        #writeLines(helptext)
        helper(cmdname)
    } else {
        res <- spsspkg.processcmd(oobj, args, "doloess")
    }
}

helper = function(cmdname) {
    # find the html help file and display in the default browser
    # cmdname may have blanks that need to be converted to _ to match the file
    
    fn = gsub(" ", "_", cmdname, fixed=TRUE)
    thefile = Find(file.exists, file.path(.libPaths(), fn, "markdown.html"))
    if (is.null(thefile)) {
        print("Help file not found")
    } else {
        browseURL(paste("file://", thefile, sep=""))
    }
}
if (exists("spsspkg.helper")) {
assign("helper", spsspkg.helper)
}