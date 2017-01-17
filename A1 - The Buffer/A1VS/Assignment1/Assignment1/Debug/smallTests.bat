CLS

buffer.exe ass1e.pls f > ass1e.out

buffer.exe ass1.pls f > ass1fi.out

buffer.exe ass1.pls m > ass1mi.out

buffer.exe ass1.pls a > ass1ai.out

if ((FC ass1e.out MPTFAssignment1_F16/ass1e.out) == 0) (echo "The empty fixed test is successful") else (echo "The empty fixed test was unsuccesful")

if ((FC ass1fi.out MPTFAssignment1_F16/ass1fe.out) == 0) (echo "The empty fixed test is successful") else (echo "The empty fixed test was unsuccesful")

if ((FC ass1mi.out MPTFAssignment1_F16/ass1mi.out) == 0) (echo "The empty fixed test is successful") else (echo "The empty fixed test was unsuccesful")

if ((FC ass1ai.out MPTFAssignment1_F16/ass1ai.out) == 0) (echo "The empty fixed test is successful") else (echo "The empty fixed test was unsuccesful")