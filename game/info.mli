(* Info contains all the data types needed for the handling of the game *)
type battlemove = Poke of string | UseAttack of string | NoMove | FaintPoke of string | Preprocess | TurnEnd | AIMove

type stat = Attack | Defense | SpecialAttack | SpecialDefense | Speed | Accuracy | Evasion

type item = Nothing | Leftovers | ChoiceBand | LifeOrb | ChoiceSpecs | ChoiceScarf | MegaStone | MegaStoneX | MegaStoneY

type guiattack = NormMove of string | Crit of guiattack |
                  SEff of guiattack | NoEff of guiattack | HitMult of int * guiattack | BurnMove of guiattack | FreezeMove of guiattack | ParaMove of guiattack | MissMove of string | FrozenSolid |
                  Thaw of guiattack | NoFreeze of guiattack | NoBurn of guiattack | NoPara of guiattack | Para | OHKill of guiattack | ChargingMove of string * string | FlinchA |
                  Recoil of guiattack | PoisonMove of guiattack | Asleep | Wake of guiattack | Confused | BreakConfuse of guiattack | StatAttackA of stat * int * guiattack
                  | ConfuseMoveA of guiattack | Recharging of guiattack | DrainA of guiattack | UserFaintA of guiattack | NoEffAll of string | DrainSleepFail of string
                  | BreakSub of guiattack | SubDmg of guiattack | ProtectedA of string | StatBoostA of stat * int * guiattack | SwitchOutA of guiattack | FalseSwipeA of guiattack
                  | ConfuseUserA of guiattack | KnockedOff of item * guiattack | SleepMove of guiattack | SleepAttack of guiattack | SleepAttackFail of string
                  | TrappingMove of guiattack | NoRecoil of guiattack | LifeOrbA of guiattack | RapidSpinA of guiattack | HitSelf of guiattack

type guistatus = StatBoost of stat * int * guistatus | NormStatus of string | ThawS of guistatus | FrozenSolidS | MissStatus of string | NoFreezeS of guistatus | NoBurnS of guistatus |
                  NoParaS of guistatus | ParaS | SwitchOut of guistatus | FlinchS | StatAttack of stat * int * guistatus | AsleepS | WakeS of guistatus | MakeSleep of guistatus
                  | ConfusedS | BreakConfuseS of guistatus | ConfuseMove of guistatus | LeechS of guistatus | PoisonStatus of guistatus | ParaStatus of guistatus
                  | BadPoisonStatus of guistatus | HealHealth of guistatus | LightScreenS of guistatus | HazeS of guistatus | ReflectS of guistatus | RestS of guistatus
                  | SubBlock of guistatus | SubFail of guistatus | SubMake of guistatus | ProtectedS of string | ProtectS of guistatus | ProtectFail of guistatus |
                   Fail of string | SpikesS of guistatus | BurnStatus of guistatus | HealBellS of guistatus | RefreshS of guistatus | PsychUpS of guistatus | SunnyDayS of guistatus |
                   RainDanceS of guistatus | SandStormS of guistatus | HailS of guistatus | EncoreS of guistatus | EncoreFail | CopyPrevMoveS of guistatus | CopyPrevMoveA of guiattack
                   | CopyFail | TauntS of guistatus | TauntFail | Taunted of string | StealthRockS of guistatus | ToxicSpikesS of guistatus | StickyWebS of guistatus
                   | SleepTalkS of guistatus * guistatus | SleepTalkA of guistatus * guiattack | SleepAttackS of guistatus | UserFaintS of guistatus | RandMoveS of guistatus | RandMoveA of guiattack |
                   ItemSwapS of guistatus | WishS of guistatus | AbilityChangeS of guistatus

type endMove = BurnDmg | BreakBurn | BreakFreeze  | BreakPara  | BreakPoison | PoisonDmg | LeechDmg of endMove | LeechHeal of endMove | Base | LightScreenFade of endMove |
               ReflectFade of endMove | SunFade of endMove | RainFade of endMove | SandStormFade of endMove | SandBuffetB of endMove | SandBuffet1 of endMove |
               SandBuffet2 of endMove | HailFade of endMove | HailBuffetB of endMove | HailBuffet1 of endMove | HailBuffet2 of endMove | TauntFade of endMove |
               TrapDamage of string * endMove | LeftOversHeal of endMove | WishEnd of endMove

type guimove = SPoke of string*string | AttackMove of guiattack | Faint | NoAction | Continue | Next | Status of guistatus | EndMove of endMove | FaintNext | SFaint | ForceChoose of guiattack | ForceMove of string
                | ForceNone | ForceChooseS of guistatus

type playerMove = Pl1 of guimove | Pl2 of guimove

type target = SpecificPoke | SelectedPokeMeFirst | Ally | UsersField |
    UserOrAlly | OpponentsFields | User | RandomOpp | AllOthers |
    SelectedPoke | AllOpp | EntireField | UserAndAlly | All

type dmg_class = Physical | Special | Status

type element = Fire | Water | Grass | Rock | Ground | Fairy | Dark | Electric |
  Ghost | Steel | Normal | Bug | Flying | Psychic | Ice | Dragon | Fighting |
  Poison

type flag = Contact | Charge | Protect | Recharge | Reflectable | Snatch |
            Mirror | Punch | Sound | Gravity | Defrost | Distance | Heal |
            Authentic | Powder | Bite | Pulse | Ballistics | Mental |
            NonSkyBattle

type evs = {attack:int; defense:int; special_attack: int; special_defense: int;
            hp:int; speed:int}

(* variants containing all secondary effects of a given move *)
type secondary_effects = MultHit of int | StageBoost of (stat * int) list | IncCrit of int | RandMultHit | BurnChance | FreezeChance | ParaChance | OHKO | ChargeMove of string |
                         ForceSwitch | FlinchMove | StageAttack of (stat * int) list | RecoilMove | PoisonChance | PutToSleep | ConfuseOpp | ConstantDmg of int | RechargeMove |
                         WeightDamage | DrainMove | LeechSeed | ChargeInSunlight of string | ToxicChance | StageBoostSunlight of (stat * int) list | Recovery | LightScreenMake
                         | Haze | ReflectMake | UserFaint | NeverMiss | DrainMoveSleep | VariableDamage | Rest | SuperFang | SubstituteMake | Flail | Protect | BellyDrum
                         | Spikes | HealBell | SunHeal | MaxHealthDmg | SunnyDay | FalseSwipe | Refresh | PsychUp | RandStageBoost | FlowerShield | RainDance | SandStormMake
                         | HailMake | Encore of int | PainSplit | SelfEncore | ConfuseUser | CopyPrevMove | KnockOff | ChanceStageBoost | SelfSwitch | FoulPlay | TauntMove
                         | StealthRockMake | TSpikes | StickyWebMake | Rototiller | SleepEffect | BeatUp | CausePartialTrapping | DoublePower | StoredPower | VenomDrench
                         | RandMove | ElectroBall | RapidSpin | ItemSwitch | WishMake | ChancePower | AbilityChange | ReverseStats | FinalGambit | GyroBall

type move = {name:string; priority: int; target: target; dmg_class: dmg_class;
    mutable power:int; effect_chance: int; accuracy: int; element: element;
    description: string; secondary: secondary_effects list}

type non_volatile_status = Burn | Freeze | Paralysis | Poisoned | Toxic of int | Sleep of int |
                          NoNon

type volatile_status =  Confusion of int | Flinch | Leeched
  | Charge | Substitute of int | Protected | UsedProtect
  | RechargingStatus | ForcedMove of int * string | ForcedMoveNoSwitch of int * string |
  Taunt of int | PartialTrapping of string * int

type status = non_volatile_status * volatile_status list

type nature = Hardy | Lonely | Adamant | Naughty | Brave | Bold | Docile |
              Impish | Lax | Relaxed | Modest | Mild | Bashful | Rash | Quiet
              | Calm | Gentle | Careful | Quirky | Sassy | Timid | Hasty |
              Jolly | Naive | Serious

type pokemon = {name: string; element: element list; move1: move; move2: move;
  move3: move; move4: move; hp: int; attack: int; defense:int;
  special_defense:int; special_attack:int; speed:int; ability:string; evs: evs;
  nature: nature; item: item}

type battle_poke = {pokeinfo: pokemon; mutable curr_hp:int; mutable curr_status: status;
  mutable curr_item: item; bhp:int; battack:int; bdefense:int; bspecial_attack:int;
  bspecial_defense:int; bspeed:int; mutable curr_abil:string}

(* stat modifier represents number of stages followed by multiplier *)
type stat_modifier = int * float

type pokemon_stat_modifier = {mutable attack: stat_modifier; mutable defense: stat_modifier;
    mutable speed: stat_modifier; mutable special_attack: stat_modifier;
    mutable special_defense: stat_modifier; mutable evasion: stat_modifier;
    mutable accuracy: stat_modifier}

type terrain_element = LightScreen of int | Reflect of int | Spikes of int | StealthRock | ToxicSpikes of int | StickyWeb | Wish of int*int | BatonPass of pokemon_stat_modifier * volatile_status list

type terrain = {side1: terrain_element list ref; side2: terrain_element list ref}

type weather = HarshSun of int | Hail of int | Rain of int | SandStorm of int
  | HeavyRain of int | Sun of int | AirCurrent of int | ClearSkies

type weather_terrain = {mutable weather: weather; terrain: terrain}

type trainer_team = {mutable current: battle_poke; mutable alive: battle_poke list; mutable dead:
                        battle_poke list; mutable stat_enhance: pokemon_stat_modifier}

(* As in competitive Pokemon, there will be no ties. If all Poke die in
one turn, the one with the Poke standing the latest wins *)
type outcome = WinnerP1 | WinnerP2

type battle_mode = Random1p | Random2p | Preset1p of pokemon list | TournBattle of pokemon list

type screen = SwitchPoke | ChooseMove | Faint | BothFaint | SwitchPokeF

type battle_state = InGame of trainer_team * trainer_team * weather_terrain * playerMove ref * playerMove ref | Loading | P1 of screen| P2 of screen | Processing

type game_state = MainMenu | Menu1P | Quit | Battle of battle_state | Menu2P | Menu0P | Preset1PChoose | Tourney | TourneyChoose | PokeEdit | PokeEditor