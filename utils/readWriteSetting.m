function [outputArg1] = readWriteSetting()
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here


% Change anything not set correctly in
%
%   "marbl_settings_set_defaults_general_parms()"
% 
%  e.g.
% select Carbon isotope option, surface flux types

mex_marbl_driver('put setting', 'ciso_on = .true.')

% mex_marbl_driver('put setting', 'lsource_sink = .true.')
% mex_marbl_driver('put setting', 'ciso_lsource_sink .true.')
% 
% FIXME: next 3 crash if called with anything but true, default is true...
% mex_marbl_driver('put setting', 'lflux_gas_o2 = .true.')
% mex_marbl_driver('put setting', 'lflux_gas_co2 = .true.')
% mex_marbl_driver('put setting', 'lcompute_nhx_surface_emis = .true.')
% 
% mex_marbl_driver('put setting', 'lvariable_PtoC = .false.')
% FIXME: crashes if called with true or false...
% mex_marbl_driver('put setting', 'init_bury_coeff_opt = "settings_file"')
% mex_marbl_driver('put setting', 'ladjust_bury_coeff = .true.')


%  **************** read and write the "settings file" easy if tedious code


% should parse "settings_file" and make a bunch of calls like the following:
% 
% In particular just read each line of 
% 
%       "marbl_with_restore.input" 
% 
%   copy the code from mex_marbl_driver.F90 .... 
%   
% 
%  do while(ioerr .eq. 0)
% 
%       call marbl_instance%put_setting(input_line)
% 
%       if (marbl_instance%StatusLog%labort_marbl) then
%         call marbl_instance%StatusLog%log_error_trace("put_setting(input_line)", subname)
%         call print_marbl_log(marbl_instance%StatusLog)
%       end if
%   end foreach
%
%
% Conversely this is the loop to save the settings to a file
% 
%     do n=1,marbl_instance%get_settings_var_cnt()
% 
%         call marbl_instance%get_setting(varname, input_line, linput_file_format=.true.)
% 
%         if (marbl_instance%StatusLog%labort_marbl) exit
%         call driver_status_log%log_noerror(input_line, subname)
%     end do


%  ***

outputArg1 = 0;

end

