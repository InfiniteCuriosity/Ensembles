#' adult—A widely used data set to predict if a person makes over $50,000/year. Automatic solution is possible using logistic analysis in the Ensembles package
#'
#' @description
#' A very widely used data set to predict if a person earns over $50,000 per year. This is also known as the "Census Income" data set.
#' It is multivariate, using both categorical and integer values.
#' Additional infomation provided from the source:
#' "Additional Information
#' Extraction was done by Barry Becker from the 1994 Census database.  A set of reasonably clean records was extracted using the following conditions: ((AAGE>16) && (AGI>100) && (AFNLWGT>1)&& (HRSWK>0))
#' Prediction task is to determine whether a person makes over 50K a year.

#' @format ## `Adult`
#' A data set with 48842 rows and 14 columns
#'
#' \describe{
#'  \item{age}{a person's age in years}
#'  \item{workclass}{categorial, based on income. Values include: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.}
#'  \item{fnlwgt}{integer used in the calculations}
#'  \item{education}{categorical, lists education level. Values include: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.}
#'  \item{education-num}{integer, gives the numerical value of a person's education}
#'  \item{maritalstatus}{categorical, values include: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.}
#'  \item{occupation}{categorical, values include: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.}
#'  \item{relationship}{categorical, values include: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried}
#'  \item{race}{categorical, values include: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black}
#'  \item{sex}{binary, values include female, male}
#'  \item{capital-gain}{integer}
#'  \item{capital-loss}{integer}
#'  \item{hours-per-week}{integer}
#'  \item{native-country}{categorical, values include: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica}
#'  \item{native-country}{categorical, values include: Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands}
#'  \item{income}{binary, <50k, >=50k}
#'
#' }
#'
#' @source <https://archive.ics.uci.edu/dataset/2/adult>
"adult"


#' dry_beans_small-A small version of the entire dry beans data set
#'
#' @description
#' This is a famous data set used for classification. This example is a stratified sample of the full data set.
#'
#' \describe{
#' \item{Area}{The area of a bean zone and the number of pixels within its boundaries}
#' \item{Perimeter}{Bean circumference is defined as the length of its border}
#' \item{MajorAxisLength}{The distance between the ends of the longest line that can be drawn from a bean}
#' \item{MinorAxisLength}{The longest line that can be drawn from the bean while standing perpendicular to the main axis}
#' \item{AspectRatio}{Defines the relationship between MajorAxisLength and MinorAxisLength}
#' \item{Eccentricity}{Eccentricity of the ellipse having the same moments as the region}
#' \item{ConvexArea}{Number of pixels in the smallest convex polygon that can contain the area of a bean seed}
#' \item{EquivDiameter}{Equivalent diameter: The diameter of a circle having the same area as a bean seed area}
#' \item{Extent}{The ratio of the pixels in the bounding box to the bean area}
#' \item{Solidity}{Also known as convexity. The ratio of the pixels in the convex shell to those found in beans}
#' \item{Roundness}{Calculated with the following formula: (4piA)/(P^2)}
#' \item{Compactness}{Measures the roundness of an object}
#' \item{ShapeFactor1}{ShapeFactor1}
#' \item{ShapeFactor2}{ShapeFactor2}
#' \item{ShapeFactor3}{ShapeFactor3}
#' \item{ShapeFactor4}{ShapeFactor4}
#' \item{Class}{(Seker, Barbunya, Bombay, Cali, Dermosan, Horoz and Sira)}
#' }
#' @source: https://archive.ics.uci.edu/dataset/602/dry+bean+dataset
"dry_beans_small"
#'
#'
#' Concrete_Data—A data set to measure the compressive strength of concrete. Automatic solution as a numerical problem in the Ensembles package.
#'
#' @description
#' This is sometimes called the concrete compressive strength data set.
#'
#' \describe{
#'  \item{Cement}{quantitative, kg in a m^3 mixture}
#'  \item{Blast_Furnace_Slag}{quantitative,  kg in a m^3 mixtur}
#'  \item{Fly_Ash}{quantitative, kg in a m^3 mixture}
#'  \item{Water}{quantitative, kg in a m^3 mixture}
#'  \item{Superplasticizer}{quantitative, kg in a m^3 mixture}
#'  \item{Coarse_Aggregate}{quantitative, kg in a m^3 mixture}
#'  \item{Fine_Aggregate}{quantitative, kg in a m^3 mixture}
#'  \item{Age}{quantitative, Day, 1~365}
#'  \item{Strength}{quantitative, measured in MPa}
#' }
#' @source <https://archive.ics.uci.edu/dataset/165/concrete+compressive+strength>
"Concrete_Data"
#'
#'
#'
#'  lebron—A logistic data set, with the result indicating whether or not LeBron scored on each shot in the data set. Automatically solvable using the Ensembles package.
#'
#'  This dataset opens the door to the intricacies of the 2023 NBA season, offering a profound understanding of the art of scoring in professional basketball.
#'
#'  \describe{
#'  \item{top}{The vertical position on the court where the shot was taken}
#'  \item{left}{The horizontal position on the court where the shot was taken}
#'  \item{date}{The date when the shot was taken. (e.g., Oct 18, 2022)}
#'  \item{qtr}{The quarter in which the shot was attempted, typically represented as "1st Qtr," "2nd Qtr," etc.}
#'  \item{time_remaining}{The time remaining in the quarter when the shot was attempted, typically displayed as minutes and seconds (e.g., 09:26).}
#'  \item{result}{Indicates whether the shot was successful, with "TRUE" for a made shot and "FALSE" for a missed shot}
#'  \item{shot_type}{Describes the type of shot attempted, such as a "2" for a two-point shot or "3" for a three-point shot}
#'  \item{distance_ft}{The distance in feet from the hoop to where the shot was taken}
#'  \item{lead}{Indicates whether the team was leading when the shot was attempted, with "TRUE" for a lead and "FALSE" for no lead}
#'  \item{lebron_team_score}{The team's score (in points) when the shot was taken}
#'  \item{opponent_team_score}{The opposing team's score (in points) when the shot was taken}
#'  \item{opponent}{The abbreviation for the opposing team (e.g., GSW for Golden State Warriors)}
#'  \item{team}{The abbreviation for LeBron James's team (e.g., LAL for Los Angeles Lakers)}
#'  \item{season}{The season in which the shots were taken, indicated as the year (e.g., 2023)}
#'  \item{color}{Represents the color code associated with the shot, which may indicate shot outcomes or other characteristics (e.g., "red" or "green")}
#'  }
#'
#'  @source <https://www.kaggle.com/datasets/dhavalrupapara/nba-2023-player-shot-dataset>
"lebron"
#'
#'
#'
#' Boston_Housing—This is a modified version of the famous Boston housing data set. The first five rows have been removed. This data set can be used with the NewBoston data set to show predictions on new data using the numerical function in the Ensembles package.
#'
#' This is a modified version of the famous Boston housing data set. The first five rows have been removed, we will use those to make predictions on new data. The data here is complete except for the first five rows.
#' The data first appeared in a paper by David Harrison, Jr. and Daniel L. Rubenfeld, Hedonic housing Prices and the demand for clean air. This was published in March, 1978.
#' Journal of Environmental Economics and Management 5(1):81-102. The descriptions below are quoted from the original paper:
#' \describe{
#' \item{crim}{Crime rate by town. Original data in 1970 FBI data}
#' \item{zn}{Proportion of a town's residential land zoned for lots greater than 25,000 square feet}
#' \item{indus}{Proportional non-retail business per town}
#' \item{chas}{Captures the amenities of a riverside location and thus should be positive}
#' \item{nox}{Nitrogen oxygen concentrations in part per hundred million}
#' \item{rm}{Average number of rooms in owner units}
#' \item{age}{Proportion of owner units built prior to 1940}
#' \item{dis}{Weighted distances to five employment centers in the Boston region}
#' \item{rad}{Index of accessibility to radial highways}
#' \item{tax}{Full property value tax rate ($/$10,000)}
#' \item{ptratio}{Pupil-teacher ratio by town school district}
#' \item{black}{Black proportion of population}
#' \item{lstat}{Proportion of population that is lower status (proportion of adults without some high school education and proportion of male workers classified as laborers)}
#' \item{medv}{Median value of owner occupied homes, from the 1970 United States census}
#' }
#'
#' @source <https://www.researchgate.net/publication/4974606_Hedonic_housing_prices_and_the_demand_for_clean_air>
"Boston_Housing"


#' NewBoston—This is the first five rows of the original Boston Housing data set. This can be used as new data, and the Boston data set as the original. The numerical function will return predictions on the new data.
#'
#' @description
#' This is the first five rows of the Boston housing data set, which have been removed from the Boston data set included here. It is otherwise identical to the Boston data set.
#'
#' \describe{
#' \item{crim}{Crime rate by town. Original data in 1970 FBI data}
#' \item{zn}{Proportion of a town's residential land zoned for lots greater than 25,000 square feet}
#' \item{indus}{Proportional non-retail business per town}
#' \item{chas}{Captures the amenities of a riverside location and thus should be positive}
#' \item{nox}{Nitrogen oxygen concentrations in part per hundred million}
#' \item{rm}{Average number of rooms in owner units}
#' \item{age}{Proportion of owner units built prior to 1940}
#' \item{dis}{Weighted distances to five employment centers in the Boston region}
#' \item{rad}{Index of accessibility to radial highways}
#' \item{tax}{Full property value tax rate ($/$10,000)}
#' \item{ptratio}{Pupil-teacher ratio by town school district}
#' \item{black}{Black proportion of population}
#' \item{lstat}{Proportion of population that is lower status (proportion of adults without some high school education and proportion of male workers classified as laborers)}
#' \item{medv}{Median value of owner occupied homes, from the 1970 United States census}
#' }
#'
#' @source <https://www.researchgate.net/publication/4974606_Hedonic_housing_prices_and_the_demand_for_clean_air>
"NewBoston"


#' diabetes—A logistic data set, determining whether a woman tested positive for diabetes. 100 percent accurate results are possible using the logistic function in the Ensembles package.
#'
#' @description
#' "This dataset is originally from the National Institute of Diabetes and Digestive and Kidney Diseases. The objective of the dataset
#' is to diagnostically predict whether or not a patient has diabetes, based on certain diagnostic measurements included in the dataset."
#'
#' \describe{
#' This data set is from www.kaggle.com. The original notes on the website state:
#' Context
#' "This dataset is originally from the National Institute of Diabetes and Digestive and Kidney Diseases. The objective of the dataset
#' is to diagnostically predict whether or not a patient has diabetes, based on certain diagnostic measurements included in the dataset.
#' Several constraints were placed on the selection of these instances from a larger database. In particular, all patients here are females
#' at least 21 years old of Pima Indian heritage."
#' Content
#' "The datasets consists of several medical predictor variables and one target variable, Outcome. Predictor variables includes the
#' number of pregnancies the patient has had, their BMI, insulin level, age, and so on.
#' Acknowledgements
#' Smith, J.W., Everhart, J.E., Dickson, W.C., Knowler, W.C., & Johannes, R.S. (1988). Using the ADAP learning algorithm to forecast the onset of diabetes mellitus.
#' In Proceedings of the Symposium on Computer Applications and Medical Care (pp. 261--265). IEEE Computer Society Press.
#'
#' \item{Pregnancies}{Number of time pregnant}
#' \item{Glucose}{Plasma glucose concentration a 2 hours in an oral glucose tolerance test}
#' \item{BloodPressure}{Diastolic blood pressure (mm Hg)}
#' \item{SkinThickness}{Triceps skin fold thickness (mm)}
#' \item{Insulin}{2-Hour serum insulin (mu U/ml)}
#' \item{BMI}{Body mass index (weight in kg/(height in m)^2)}
#' \item{DiabetesPedigreeFunction}{Diabetes pedigree function}
#' \item{Age}{Age (years)}
#' \item{Outcome}{Class variable (0 or 1) 268 of 768 are 1, the others are 0}
#' }
#'
#' @source <https://www.kaggle.com/datasets/uciml/pima-indians-diabetes-database/data>
"diabetes"

#' total_nonfarm—This is a time series data set. It lists the total number of nonfarm employees per month, starting in 1996. The forecasting function in the Ensembles package can be used to make time series forecasts, such as the total nonfarm employees.
#'
#' @description
#' This data set is from the Bureau of Labor Statistics. The Bureau reports the number of total nonfarm employees in the United States on the first Friday of each month (unless that day is a federal holiday).

#' \describe{
#' This data set is from: https://data.bls.gov/timeseries/CES0000000001, and is modified to include the 1-month change in total nonfarm employment.
#' The data consists of two columns, one to establish the date (such as Jan 2021), and the other to establish the total number of nonfarm employees that month.
#'
#' \item{label}{the label for the specific time, such as 2022 Nov}
#' \item{Value}{the quantity for that specific time, such as 154296}
#' }
#'
#' @source <https://data.bls.gov/timeseries/CES0000000001>
"total_nonfarm"

#' Monthly_Labor-This is a set of monthly time series reports from the United States Bureau of Labor Statistics. It lists the number of employees in each of the following areas:
#' The current month, All_Employees, Construction, Education, Finance, Government, Information, Hospitality, Manufacturing, Oil_and_Gas, Retail, Temporary, and the Unemployment rate.
#'
#' @description
#' These are 13 time series reports from the United States Bureau of Labor Statistics, all in one document.
#'
#' \describe{
#' The original data is from the United States Department of Labor Statistics. The data consists of 14 columns: A Month column, 12 sections of the United States economy (such as Construction or Education),
#' and the total unemployment rate for the nation.
#'
#' \item{Month}{The month being reported, starting with January 2006}
#' \item{All_Employees}{The total number of non-farm employees for a specific month, series number: CES0000000001}
#' \item{Construction}{The number of people working in Construction for that month, series number: CES2000000001}
#' \item{Education}{The number of people working in Education for that month, series number: CES6500000001}
#' \item{Finance}{The number of people working in Finance for that month, series number: CES5500000001}
#' \item{Government}{The number of people working in government for that month, series number: CES9000000001}
#' \item{Information}{The number of people working in Information for that month, series number: CES5000000001}
#' \item{Hospitality}{The number of people working in Hospitality for that month, series number: CES7000000001}
#' \item{Manufacturing}{The number of people working in Manufacturing for that month, series number: CES3000000001}
#' \item{Oil_and_Gas}{The number of people working in Oil and Gas for that month, series number: CES1021100001}
#' \item{Prof_Svcs}{The number of people working in Professional Services for that month, series number: CES6000000001}
#' \item{Tempoary}{The number of people working in the tempoary staffing industry that month, series number: CES6056132001}
#' \item{Unemployment}{The national (United States) unemployment rate for that month, LNS14000000}
#' }
#' @source <https://data.bls.gov/>
"Monthly_Labor"

#'
#' Monthly_Labor_Change-This is a set of 1-month change reports for the monthly time series reports from the United States Bureau of Labor Statistics. It lists the 1-month change in the number of employees in each of the following areas:
#' The current month, All_Employees, Construction, Education, Finance, Government, Information, Hospitality, Manufacturing, Oil_and_Gas, Retail, Temporary, and the Unemployment rate.
#'
#' @description
#' These are 13 time series reports from the United States Bureau of Labor Statistics, all in one document.
#'
#' \describe{
#' The original data is from the United States Department of Labor Statistics. The data consists of 14 columns: A Month column, 12 sections of the United States economy (such as Construction or Education),
#' and the total unemployment rate for the nation. This data reports the one-month change in the data
#'
#' \item{Month}{The month being reported, starting with January 2006}
#' \item{All_Employees}{The total number of non-farm employees for a specific month, series number: CES0000000001}
#' \item{Construction}{The number of people working in Construction for that month, series number: CES2000000001}
#' \item{Education}{The number of people working in Education for that month, series number: CES6500000001}
#' \item{Finance}{The number of people working in Finance for that month, series number: CES5500000001}
#' \item{Government}{The number of people working in government for that month, series number: CES9000000001}
#' \item{Information}{The number of people working in Information for that month, series number: CES5000000001}
#' \item{Hospitality}{The number of people working in Hospitality for that month, series number: CES7000000001}
#' \item{Manufacturing}{The number of people working in Manufacturing for that month, series number: CES3000000001}
#' \item{Oil_and_Gas}{The number of people working in Oil and Gas for that month, series number: CES1021100001}
#' \item{Prof_Svcs}{The number of people working in Professional Services for that month, series number: CES6000000001}
#' \item{Tempoary}{The number of people working in the tempoary staffing industry that month, series number: CES6056132001}
#' \item{Unemployment}{The national (United States) unemployment rate for that month, LNS14000000}
#'  }
#' @source <https://data.bls.gov/>
"Monthly_Labor_Change"
