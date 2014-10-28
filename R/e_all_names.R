
experiments <- list(
  exp_e103 = list(
    eegfile1200 = c('e103S001R07','e103S001R08','e103S001R11','e103S001R12'),
    edffile1200 = c('23476936', '23476943', '23476958', '23476964'),
    eegfile800 = c('e103S001R02', 'e103S001R05', 'e103S001R06'),
    edffile800 = c('23476900', '23476916', '23476925')
  ),
  exp_e105 = list(
    eegfile1200 = c('e105S001R06','e105S001R07','e105S001R08','e105S001R09'),
    edffile1200 = c('23478296','23478303', '23478310', '23478316'),
    eegfile800 = c('e105S001R10','e105S001R11','e105S001R13'),
    edffile800 = c('23478323', '23478329', '23478341')
  ),
  exp_e106 = list(
    eegfile1200 = c('e106S001R08','e106S001R13','e106S001R14'),
    edffile1200 = c('23478529','23478554', '23478561'),
    eegfile800 = c('e106S001R01','e106S001R02','e106S001R05'),
    edffile800 = c('23478466','23478475', '23478517')
  ),
  exp_e107 = list(
    eegfile1200 = c('e107S001R05','e107S001R07','e107S001R08'),
    edffile1200 = c('23482777','23482795', '23482802'),
    eegfile800 = c('e107S001R01','e107S001R02','e107S001R03', 'e107S001R04'),
    edffile800 = c('23482744', '23482751', '23482758', '23482765')
))


# for superplot 


exp202 <- list(
  file1 = "23525924",
  file2 = "23525948",
  file3 = "23525957",
  file4 = "23525993"
  )

exp203 <- list(
  file1 = "23537434",
  file2 = "23537452",
  file3 = "23537464",
  file4 = "23537478",
  file5 = "23537491",
  file6 = "23537507"
)

exp204 <- list(
  file23566369 = '23566333',
  file23566369 = '23566369',
  file23566382 = '23566382',
  file23566390 = '23566390')

j.files800 <- list(
  f23476900 = "23476900",
  f23476916 =	"23476916",
  f23476925 =	"23476925",
  f23478323 =	"23478323",
  f23478329 =	"23478329",
  f23478341 =	"23478341",
  f23478466 = "23478466",
  f23478475 =	"23478475",
  f23478517 =	"23478517",
  f23482744 = "23482744",
  f23482751 =	"23482751",
  f23482758 =	"23482758",
  f23482765 =	"23482765"  
  )

j.files1200 <- list(
  f23476936 =  "23476936",
  f23476943 =	"23476943",
  f23476958 =	"23476958",
  f23476964 =	"23476964",
  f23478296 =	"23478296",
  f23478303 =	"23478303",
  f23478310 =	"23478310",
  f23478316 =	"23478316",
  f23478529 =	"23478529",
  f23478554 =	"23478554",
  f23478561 =	"23478561",
  f23482777 =	"23482777",
  f23482795 =	"23482795",
  f23482802 =	"23482802"
  )

all.800 <- function(files)
{
  for(file in files)
  {
    superplot.j(file, 800)
  }
}

all.1200 <- function(files)
{
  for(file in files)
  {
    superplot.j(file, 1200)
  }
}

super.all <- function(exp)
{
  for(file in exp)
  {
    superplot(file, 'btn')
    superplot(file, 'fixation')
    superplot(file, 'ball')
    superplot(file, 'moveTo')
    superplot(file, 'inBlocked')
  }
}

diff.block.all <- function(exp)
{
  for (file in exp)
  {
    superplot(file, 'btn')
    superplot(file, 'fixation')
    superplot(file, 'ball')
    superplot(file, 'moveTo')
    superplot(file, 'block_ball')
    superplot(file, 'block_board')
  }
}

#for check.sync


files202 <- list(
  list(edffile = "23525924", eegfile = "e201S001R03"),
  list(edffile = "23525948", eegfile = "e202S001R01"),
  list(edffile = "23525957", eegfile = "e202S001R02"),
  list(edffile = "23525993", eegfile = "e202S001R04"))

files203 <- list(
  list(edffile = "23537434", eegfile = "e203S001R01"),
  list(edffile = "23537452", eegfile = "e203S001R02"),
  list(edffile = "23537478", eegfile = "e203S001R03"),
  list(edffile = "23537507", eegfile = "e203S001R05"))

files204 <- list(
  list(edffile = "23566333", eegfile = "e204S001R02"),
  list(edffile = "23566369", eegfile = "e204S001R03"),
  list(edffile = "23566382", eegfile = "e204S001R04"),
  list(edffile = "23566390", eegfile = "e204S001R05"))

files103 <- list(
  list(edffile = "23476936", eegfile = "e103S001R07"),
  list(edffile = "23476943", eegfile = "e103S001R08"),
  list(edffile = "23476958", eegfile = "e103S001R11"),
  list(edffile = "23476964", eegfile = "e103S001R12"),  
  list(edffile = "23476900", eegfile = "e103S001R02"),
  list(edffile = "23476916", eegfile = "e103S001R05"),
  list(edffile = "23476925", eegfile = "e103S001R06"))
  

files105 <- list(
  list(edffile = "23478296", eegfile = "e105S001R06"),
  list(edffile = "23478303", eegfile = "e105S001R07"),
  list(edffile = "23478310", eegfile = "e105S001R08"),
  list(edffile = "23478316", eegfile = "e105S001R09"),  
  list(edffile = "23478323", eegfile = "e105S001R10"),
  list(edffile = "23478329", eegfile = "e105S001R11"),
  list(edffile = "23478341", eegfile = "e105S001R13"))  
  
  
 files106 <- list(
   list(edffile = "23478529", eegfile = "e106S001R08"),
   list(edffile = "23478554", eegfile = "e106S001R13"),
   list(edffile = "23478561", eegfile = "e106S001R14"),
   list(edffile = "23478466", eegfile = "e106S001R01"),
   list(edffile = "23478475", eegfile = "e106S001R02"),
   list(edffile = "23478517", eegfile = "e106S001R05"))   

  files107 <- list(
    list(edffile = "23482777", eegfile = "e107S001R05"),
    list(edffile = "23482795", eegfile = "e107S001R07"),
    list(edffile = "23482802", eegfile = "e107S001R08"),  
    list(edffile = "23482744", eegfile = "e107S001R01"),
    list(edffile = "23482751", eegfile = "e107S001R02"),
    list(edffile = "23482758", eegfile = "e107S001R03"),
    list(edffile = "23482765", eegfile = "e107S001R04"))   


