function [name] = diag_names(diag_name, diag_cnt)

% this -does- work after "shutdown" but -not- before "init"

name = cell(diag_cnt,1);

for i = 1:diag_cnt
    name{i} = mex_marbl_driver('diag_sname', diag_name, i);
end

end
