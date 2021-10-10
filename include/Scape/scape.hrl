% Define the internal state of a scape
-record(state,{dataset,	
                has_header, % flag indicate if the dataset have an header
                dataset_actions, % actions on dataset see record below
                current, % current record in evaluation
                cursor, % cursor on the dataset
                num_line_readed, % number of readed line
                limit, % number limit of readable line during fit session
                info, % information about dataset 
                matrix, % confusion matrix, used only in the classification scape
                fit, % fit evaluation
                loss, % loss evaluation
                labelled % boolean indicates if the record have a target or not
            }).

% Define action applied on a dataset from the scape
%% read_action = ReadFun(Dataset, Cursor) -> {NextLine, UpdatedCursor}
%% reset_action = ResetFun(Dataset, Cursor) -> {StartDataset, InitCursor}
%% parse_line_action = ParseLineFun(Line) -> ParsedLine
%% is_finished_action = IsFinishedFun(Dataset, Cursor) -> Boolean
-record(dataset_actions,{
                 read_action, %% read next line from the dataset
                 parse_line_action, %parse a line readed from the dataset
                 is_finished_action, % evaluate if the dataset if finished
                 reset_action %return at the start of the dataset
                }).