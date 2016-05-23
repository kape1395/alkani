
-record(player, {
    name    :: binary(),    %
    pid     :: pid(),       % Player's process ID.
    pos_x   :: number(),    % Position X \in [0..1)
    pos_y   :: number(),    % Position Y \in [0..1)
    size    :: integer(),   %
    dir     :: number() | undefined
}).

-record(food, {
    pos_x   :: number(),    % Position X \in [0..1)
    pos_y   :: number(),    % Position Y \in [0..1)
    size    :: integer(),   %
    dir     :: number()     % Radians.
}).

