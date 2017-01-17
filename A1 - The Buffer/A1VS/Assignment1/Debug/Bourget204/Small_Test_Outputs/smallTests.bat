CLS

Buffer.exe ass1e.pls 200 15 f y > ass1e.out

Buffer.exe ass1.pls 200 15 f y > ass1fi.out

Buffer.exe ass1.pls 200 15 m y > ass1mi.out

Buffer.exe ass1.pls 200 15 a y > ass1ai.out

::if ((FC ass1e.out MPTFAssignment1_F16/ass1e.out) == 0) (echo "The empty fixed test is successful") else (echo "The empty fixed test was unsuccesful")

::if ((FC ass1fi.out MPTFAssignment1_F16/ass1fe.out) == 0) (echo "The empty fixed test is successful") else (echo "The empty fixed test was unsuccesful")

::if ((FC ass1mi.out MPTFAssignment1_F16/ass1mi.out) == 0) (echo "The empty fixed test is successful") else (echo "The empty fixed test was unsuccesful")

::if ((FC ass1ai.out MPTFAssignment1_F16/ass1ai.out) == 0) (echo "The empty fixed test is successful") else (echo "The empty fixed test was unsuccesful")