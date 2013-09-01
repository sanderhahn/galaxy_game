%% @doc
%% Implementation module for the galactic battle simulator.
%% The following example shows the expected behavior of the simulator:
%%
%% Planets=[mercury,uranus,venus, earth]
%% Shields=[mercury,uranus]
%% Alliances=[{mercury, uranus}, {venus, earth}]
%% Actions=[{nuclear,mercury},{laser,venus}, {laser, uranus}]
%%
%% ExpectedSurvivors = [uranus]
%% In order to produce this expected results, the following calls will be tested:
%% * ok = setup_universe(Planets, Shields, Alliances)
%% * [uranus] = simulate_attack(Planets, Actions)
%% * ok = teardown_universe(Planets)
%%
%% All the 3 calls will be tested in order to check they produce the expected
%% side effects (setup_universe/3 creates a process per planet, etc)
%% @end

-module(galaxy_game).

-include_lib("eunit/include/eunit.hrl").

-type planet()::atom().
-type shield()::planet().
-type alliance()::{planet(), planet()}.
-type attack()::{laser | nuclear, planet()}.

-export([setup_universe/3, teardown_universe/1, simulate_attack/2]).
%-compile(export_all).

%% @doc Set up a universe described by the input.
%% The imput is asumed to be minimal and non redundant (i.e. if there is an
%% alliance {a, b} there won't be an alliance {b, a}).
%% Once this function returns, the universe is expected to be fully ready,
%% shields, alliances and all.
-spec setup_universe([planet()], [shield()], [alliance()]) -> ok.
%% @end
setup_universe(Planets, Shields, Alliances) ->
    Galaxy = self(),
    setup_planets(Planets, Galaxy),
    setup_shields(Shields, Galaxy),
    setup_alliences(Alliances, Galaxy),
    ok.

%% @doc Clean up a universe simulation.
%% This function will only be called after calling setup_universe/3 with the
%% same set of planets.
%% Once this function returns, all the processes spawned by the simulation
%% should be gone.
-spec teardown_universe([planet()]) -> ok.
%% @end
teardown_universe(Planets) ->
    teardown_planets(Planets),
    ok.

%% @doc Simulate an attack.
%% This function will only be called after setting up a universe with the same
%% set of planets.
%% It returns the list of planets that have survived the attack
-spec simulate_attack([planet()], [attack()]) -> Survivors::[planet()].
%% @end
simulate_attack(Planets, Actions) ->
    lists:map(fun simulate_action/1, Actions),

    % @todo not sure how deterministally know if all simulate actions are propagated... implementation needs a better design? need to learn some more Erlang/OTP :)
    
    % Wait a bit for planets to die off.
    timer:sleep(10),

    [ Planet || Planet <- Planets , whereis(Planet) =/= undefined].

%% Not really sure if my approach with the ack messages is
%% the right way to make the galaxy synchronize on
%% the different phases.

%% It feels kind of ugly and may not be the OTP way...

setup_planets(Planets, Galaxy) ->
    [ setup_planet(Planet, Galaxy) || Planet <- Planets],
    ok.

setup_shields(Shields, Galaxy) ->
    [ setup_shield(Shield, Galaxy) || Shield <- Shields],
    ok.

setup_alliences(Alliences, Galaxy) ->
    [ setup_allience(Allience, Galaxy) || Allience <- Alliences],
    ok.

teardown_planets(Planets) ->
    [ teardown_planet(Planet) || Planet <- Planets, whereis(Planet) =/= undefined].

setup_planet(Planet, Galaxy) ->
    proc_lib:spawn(fun() ->
        register(Planet, self()),
        io:format("Planet ~p created~n", [Planet]),
        Galaxy ! ack,
        planet_loop(Planet, false)
    end),
    receive
        ack -> ok
    end,
    ok.

setup_shield(Shield, Galaxy) ->
    Planet = Shield,
    whereis(Planet) ! {shield, Galaxy},
    receive
        ack -> ok
    end,
    ok.

teardown_planet(Planet) ->
    exit(whereis(Planet), kill).

setup_allience({Planet, Partner}, Galaxy) ->
    io:format("~p ~p~n", [Planet, Partner]),
    whereis(Planet) ! {allience, Partner, Galaxy},
    receive
        ack -> ok
    end,
    ok.

planet_loop(Planet, HasShield) ->
    receive

        {shield, Galaxy} ->
            io:format("Planet ~p got a shield~n", [Planet]),
            process_flag(trap_exit, true),
            Galaxy ! ack,
            planet_loop(Planet, true);

        {allience, Partner, Galaxy} ->
            io:format("Planet ~p allied with ~p~n", [Planet, Partner]),
            link(whereis(Partner)),
            Galaxy ! ack,
            planet_loop(Planet, HasShield);

        {'EXIT', _FromPid, laser} ->
            case HasShield of
                true ->
                    io:format("Planet ~p attacked with laser~n", [Planet]),
                    planet_loop(Planet, false);
                false ->
                    io:format("Planet ~p attacked with laser and destroyed~n", [Planet])
            end;

        {'EXIT', _FromPid, nuclear} ->
            io:format("Planet ~p destroyed with canon~n", [Planet]);

        {'EXIT', _FromPid, _Reason} ->
            io:format("Planet ~p destroyed~n", [Planet])
    end.

simulate_action({laser, Planet}) ->
    io:format("Shoot laser at ~p~n", [Planet]),
    exit(whereis(Planet), laser);

simulate_action({nuclear, Planet}) ->
    io:format("Nuclear canon shot at ~p~n", [Planet]),
    exit(whereis(Planet), nuclear).

