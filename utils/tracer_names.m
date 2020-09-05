function [my_cell] = tracer_names(lciso_on)
%tracer_names MARBL "short name" of all tracers

% if MARBL is shutdown then mex_marbl_driver('tracer_sname', i) crashes...
%    just do a lot of typing...

my_cell{ 1} = 'PO4';
my_cell{ 2} = 'NO3';
my_cell{ 3} = 'SiO3';
my_cell{ 4} = 'NH4';
my_cell{ 5} = 'Fe';
my_cell{ 6} = 'Lig';
my_cell{ 7} = 'O2';
my_cell{ 8} = 'DIC';
my_cell{ 9} = 'DIC_ALT_CO2';
my_cell{10} = 'ALK';
my_cell{11} = 'ALK_ALT_CO2';
my_cell{12} = 'DOC';
my_cell{13} = 'DON';
my_cell{14} = 'DOP';
my_cell{15} = 'DOPr';
my_cell{16} = 'DONr';
my_cell{17} = 'DOCr';
my_cell{18} = 'zooC';
my_cell{19} = 'spChl';
my_cell{20} = 'spC';
my_cell{21} = 'spP';
my_cell{22} = 'spFe';
my_cell{23} = 'spCaCO3';
my_cell{24} = 'diatChl';
my_cell{25} = 'diatC';
my_cell{26} = 'diatP';
my_cell{27} = 'diatFe';
my_cell{28} = 'diatSi';
my_cell{29} = 'diazChl';
my_cell{30} = 'diazC';
my_cell{31} = 'diazP';
my_cell{32} = 'diazFe';
if lciso_on == 1
    my_cell{33} = 'DI13C';
    my_cell{34} = 'DO13Ctot';
    my_cell{35} = 'DI14C';
    my_cell{36} = 'DO14Ctot';
    my_cell{37} = 'zootot13C';
    my_cell{38} = 'zootot14C';
    my_cell{39} = 'sp13C';
    my_cell{40} = 'sp14C';
    my_cell{41} = 'spCa13CO3';
    my_cell{42} = 'spCa14CO3';
    my_cell{43} = 'diat13C';
    my_cell{44} = 'diat14C';
    my_cell{45} = 'diaz13C';
    my_cell{46} = 'diaz14C';
end
end

