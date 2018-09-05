#### get codes from https://www2.census.gov/govs/local/ summary tabulation methodology

codes_tot_rev<-c("B01", "B21", "B22", "B30", "B42", "B46", "B50", 
                 "B59", "B79", "B80", "B89", "B91", "B92", 
                 "B93", "B94", "C21", "C30", "C42", "C46", "C50", 
                 "C79", "C80", "C89", "C91", "C92", "C93", "C94", "D21", 
                 "D30", "D42", "D46", "D50", "D79", "D80", "D89", "D91", 
                 "D92", "D93", "D94", "T01", "T09", "T10", "T11", "T12", 
                 "T13", "T14", "T15", "T16", "T19", "T20", "T21", "T22", 
                 "T23", "T24", "T25", "T27", "T28", "T29", "T40", "T41", 
                 "T50", "T51", "T53", "T99", "A01", "A03", "A09", "A10", 
                 "A12", "A16", "A18", "A21", "A36", "A44", "A45", "A50", 
                 "A56", "A59", "A60", "A61", "A80", "A81", "A87", "A89", 
                 "U01", "U11", "U20", "U21", "U30", "U40", "U41", "U50", "U95", 
                 "U99", "A90", "A91", "A92", "A93", "A94", "X01", "X02", "X05", 
                 "X08", "Y01", "Y02", "Y04", "Y11", "Y12", "Y51", "Y52")

codes_gen_rev<-c("B01", "B21", "B22", "B30", "B42", "B46", 
                 "B50", "B59", "B79", "B80", "B89", "B91", 
                 "B92", "B93", "B94", "C21", "C30", "C42", 
                 "C46", "C50", "C79", "C80", "C89", "C91", 
                 "C92", "C93", "C94", "D21", "D30", "D42", 
                 "D46", "D50", "D79", "D80", "D89", "D91", 
                 "D92", "D93", "D94", "T01", "T09", "T10", 
                 "T11", "T12", "T13", "T14", "T15", "T16", 
                 "T19", "T20", "T21", "T22", "T23", "T24", 
                 "T25", "T27", "T28", "T29", "T40", "T41", 
                 "T50", "T51", "T53", "T99", "A01", "A03", 
                 "A09", "A10", "A12", "A16", "A18", "A21", 
                 "A36", "A44", "A45", "A50", "A56", "A59", 
                 "A60", "A61", "A80", "A81", "A87", "A89", 
                 "U01", "U11", "U20", "U21", "U30", "U40", 
                 "U41", "U50", "U95", "U99")

codes_int_gov_rev<-c(
  "B01", "B21", "B22", "B30", "B42", "B46", "B50", "B59", 
  "B79", "B80", "B89", "B91", "B92", "B93", "B94", "C21", 
  "C30", "C42", "C46", "C50", "C79", "C80", "C89", "C91", 
  "C92", "C93", "C94", "D21", "D30", "D42", "D46", "D50", 
  "D79", "D80", "D89", "D91", "D92", "D93", "D94")

codes_int_gov_police<-c(
  "L62", "M62")

codes_gen_rev_ownsource<-c(
  "A01", "A03", "A09", "A10", "A12", "A16", "A18", "A21", 
  "A36", "A44", "A45", "A50", "A56", "A59", "A60", "A61", 
  "A80", "A81", "A87", "A89", "T01", "T09", "T10", "T11", 
  "T12", "T13", "T14", "T15", "T16", "T19", "T20", "T21", 
  "T22", "T23", "T24", "T25", "T27", "T28", "T29", "T40", 
  "T41", "T50", "T51", "T53", "T99", "U01", "U11", "U20", 
  "U21", "U30", "U40", "U41", "U50", "U95", "U99"
)

codes_tax_rev<-c(
  "T01", "T09", "T10", "T11", "T12", "T13", "T14", "T15", 
  "T16", "T19", "T20", "T21", "T22", "T23", "T24", "T25", 
  "T27", "T28", "T29", "T40", "T41", "T50", "T51", "T53", "T99"
)

codes_prop_tax_rev<-c("T01")
codes_sales_tax_rev<-c("T09", "T10", "T11", "T12", "T13", 
                       "T14", "T15", "T16", "T19")

codes_fines_rev<-c("U30")

codes_expenses<-c(
  "E01", "E03", "E04", "E05", "E12", "E16", "E18", "E21", 
  "E22", "E23", "E24", "E25", "E26", "E29", "E31", "E32", 
  "E36", "E44", "E45", "E50", "E52", "E55", "E56", "E59", 
  "E60", "E61", "E62", "E66", "E73", "E74", "E75", "E77", 
  "E79", "E80", "E81", "E85", "E87", "E89", "E90", "E91", 
  "E92", "E93", "E94", "I89", "I91", "I92", "I93", "I94", 
  "J19", "L67", "J68", "J85", "X11", "X12", "Y05", "Y06", 
  "Y14", "Y53", "F01", "F03", "F04", "F05", "F12", "F16", 
  "F18", "F21", "F22", "F23", "F24", "F25", "F26", "F29", 
  "F31", "F32", "F36", "F44", "F45", "F50", "F52", "F55", 
  "F56", "F59", "F60", "F61", "F62", "F66", "F77", "F79", 
  "F80", "F81", "F85", "F87", "F89", "F90", "F91", "F92", 
  "F93", "F94", "G01", "G03", "G04", "G05", "G12", "G16", 
  "G18", "G21", "G22", "G23", "G24", "G25", "G26", "G29", 
  "G31", "G32", "G36", "G44", "G45", "G50", "G52", "G55", 
  "G56", "G59", "G60", "G61", "G62", "G66", "G77", "G79", 
  "G80", "G81", "G85", "G87", "G89", "G90", "G91", "G92", 
  "G93", "G94", "J67", "L01", "L04", "L05", "L12", "L18", 
  "L23", "L25", "L29", "L32", "L36", "L44", "L50", "L52", 
  "L59", "L60", "L61", "L62", "L66", "L67", "L79", "L80", 
  "L81", "L87", "L89", "L91", "L92", "L93", "L94", "M01", 
  "M04", "M05", "M12", "M18", "M21", "M23", "M24", "M25", 
  "M29", "M30", "M32", "M36", "M44", "M50", "M52", "M55", 
  "M56", "M59", "M60", "M61", "M62", "M66", "M67", "M68", 
  "M79", "M80", "M81", "M87", "M89", "M91", "M92", "M93", 
  "M94", "Q12", "Q18", "S67", "S74", "S89")

codes_welfare_expenses<-c(
  "J67", "J68", "E73", "E74", "E75", "E77", "F77", "G77", 
  "E79", "F79", "G79")

codes_police_expenses<-c(
  "E62", "F62", "G62")

codes_correction_expenses<-c(
  "E04", "F04", "G04", "E05", "F05", "G05")

#### transform functions

make_wide_cog<-function(x){
  
  out<-x%>%
    filter(item_code%in%codes_expenses)%>%
    group_by(gov_id, year)%>%
    summarise(exp_tot = sum(amount, na.rm=TRUE))%>%
  left_join(x%>%
    filter(item_code%in%codes_correction_expenses)%>%
    group_by(gov_id, year)%>%
    summarise(exp_correction = sum(amount, na.rm=TRUE)))%>%
  left_join(x%>%
    filter(item_code%in%codes_police_expenses)%>%
    group_by(gov_id, year)%>%
    summarise(exp_police = sum(amount, na.rm=TRUE)))%>%
  left_join(x%>%
    filter(item_code%in%codes_welfare_expenses)%>%
    group_by(gov_id, year)%>%
    summarise(exp_welfare = sum(amount, na.rm=TRUE)))%>%
  left_join(x%>%
    filter(item_code%in%codes_tot_rev)%>%
    group_by(gov_id, year)%>%
    summarise(rev_tot = sum(amount, na.rm=TRUE)))%>%
  left_join(x%>%
    filter(item_code%in%codes_fines_rev)%>%
    group_by(gov_id, year)%>%
    summarise(rev_fines = sum(amount, na.rm=TRUE)))%>%  
  left_join(x%>%
    filter(item_code%in%codes_gen_rev_ownsource)%>%
    group_by(gov_id, year)%>%
    summarise(rev_gen_ownsource = sum(amount, na.rm=TRUE)))%>% 
  left_join(x%>%
    filter(item_code%in%codes_int_gov_rev)%>%
    group_by(gov_id, year)%>%
    summarise(rev_int_gov = sum(amount, na.rm=TRUE)))%>%  
  left_join(x%>%
    filter(item_code%in%codes_prop_tax_rev)%>%
    group_by(gov_id, year)%>%
    summarise(rev_prop_tax = sum(amount, na.rm=TRUE)))%>%  
  left_join(x%>%
    filter(item_code%in%codes_sales_tax_rev)%>%
    group_by(gov_id, year)%>%
    summarise(rev_sales_tax = sum(amount, na.rm=TRUE)))%>%  
  left_join(x%>%
    filter(item_code%in%codes_tax_rev)%>%
    group_by(gov_id, year)%>%
    summarise(rev_tax = sum(amount, na.rm=TRUE)))%>%  
  left_join(x%>%
    filter(item_code%in%codes_int_gov_police)%>%
    group_by(gov_id, year)%>%
    summarise(rev_int_gov_police = sum(amount, na.rm=TRUE)))
  
  out[is.na(out)]<-0
  
  return(out)
 
}
