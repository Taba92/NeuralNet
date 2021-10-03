% Define the internal state of a scape
-record(state,{dataset,	
                has_header, % flag indicate if the dataset have an header
                dataset_actions, % actions on dataset see record below
                current, % current record in evaluation
                num_line_readed, % number of readed line
                limit, % number limit of readable line during fit session
                info, % information about dataset
                matrix, % confusion matrix, used only in the classification scape
                fit, % fit evalutation
                loss % loss evaluation
            }).

% Define action applied on a dataset from the scape
%% read_action = ReadFun(Dataset, NumLineReaded) -> NextLine
%% get_line_action = GetLineFun(Dataset, NumLineReaded -> CurrentLine
%% reset_action = ResetFun(Dataset, NumLineReaded) -> StartDataset
%% parse_line_action = ParseLineFun(Line) -> {Features, Target}  
%% extract_line_action = ExtractLineFun(ParsedLine) -> {Features, Target}
%% is_finished_action = IsFinishedFun(Dataset, NumLineReaded) -> Boolean
-record(dataset_actions,{read_action, %% read next line from the dataset
                 get_line_action, %% read the current line from the dataset
                 parse_line_action, %parse a line readed from the dataset
                 extract_line_action, % extract features and targets from a parsed line
                 is_finished_action, % evaluate if the dataset if finished
                 reset_action %return at the start of the dataset
                }).