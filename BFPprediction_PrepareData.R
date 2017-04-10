library( Hmisc )

NHANESnames <- c( "DEMO.XPT", "LAB18.XPT", "LAB25.XPT", "LAB11.XPT", "BMX.XPT", "BIX.XPT" )
BaseURL <- "https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/"

nhanes <- Reduce( function( ... ) merge( ... ),
                  lapply( NHANESnames, function( n ) sasxport.get( paste0( BaseURL, n ), lowernames = FALSE ) ) )

nhanes <- subset( nhanes, !is.na( RIDAGEYR )&RIDAGEYR>18 )
nhanes <- subset( nhanes, !is.na( RIAGENDR )&RIAGENDR==1 )

nhanes <- data.frame( ANC = nhanes$LBDNENO, ABC = nhanes$LBDBANO,
                      ALC = nhanes$LBDLYMNO, AMC = nhanes$LBDMONO,
                      AEC = nhanes$LBDEONO, RBC = nhanes$LBXRBCSI,
                      HGB = nhanes$LBXHGB * 10, HCT = nhanes$LBXHCT / 100,
                      MCV = nhanes$LBXMCVSI, MCH = nhanes$LBXMCHSI,
                      MCHC = nhanes$LBXMC * 10, RDW = nhanes$LBXRDW,
                      PLT = nhanes$LBXPLTSI, MPV = nhanes$LBXMPSI,
                      SNA = nhanes$LBXSNASI, SK = nhanes$LBXSKSI,
                      SCL = nhanes$LBXSCLSI, SCA = nhanes$LBDSCASI,
                      SP = nhanes$LBDSPHSI, STB = nhanes$LBDSTBSI,
                      BIC = nhanes$LBXSC3SI, GLU = nhanes$LBDSGLSI,
                      IRN = nhanes$LBDSIRSI, LDH = nhanes$LBXSLDSI,
                      STP = nhanes$LBDSTPSI, SUA = nhanes$LBDSUASI,
                      SAL = nhanes$LBDSALSI, TRI = nhanes$LBDSTRSI,
                      BUN = nhanes$LBDSBUSI, CRP = nhanes$LBXCRP,
                      SCR = nhanes$LBDSCRSI, STC = nhanes$LBDSCHSI,
                      AST = nhanes$LBXSASSI, ALT = nhanes$LBXSATSI,
                      GGT = nhanes$LBXSGTSI, ALP = nhanes$LBXSAPSI,
                      WT = nhanes$BMXWT, HT = nhanes$BMXHT,
                      WC = nhanes$BMXWAIST, AGE = nhanes$RIDAGEYR,
                      BFP = nhanes$BIDPFAT, WEIGHT = nhanes$WTMEC2YR )

apply( nhanes, 2, function( x ) mean( is.na( x ) ) )
table( apply( nhanes, 1, function( x ) sum( is.na( x ) ) ) )
nhanes <- na.omit( nhanes )

cairo_pdf( "BFPhist.pdf" )
histogram( nhanes$BFP, xlab = "Body fat percentage [%]", ylab = "Distribution [%]" )
dev.off()

save( nhanes, file = "nhanes.dat" )