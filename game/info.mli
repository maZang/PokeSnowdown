(* Info contains all the data types needed for the handling of the game *)
type battlemove = Poke of string | UseAttack of string | NoMove
                  | FaintPoke of string | NoPreprocess | AIMove

type stat =
  Attack | Defense | SpecialAttack | SpecialDefense | Speed | Accuracy | Evasion

type item =
  Nothing | Leftovers | ChoiceBand | LifeOrb | ChoiceSpecs | ChoiceScarf
  | MegaStone | MegaStoneX | MegaStoneY | LightBall

type guiattack =
  NormMove of string | NormStatus of string | Crit of guiattack |SEff of guiattack | NoEff of guiattack
  | HitMult of int * guiattack | BurnMove of guiattack | FreezeMove of guiattack
  | ParaMove of guiattack | MissMove of string | FrozenSolid | Thaw of guiattack
  | NoFreeze of guiattack | NoBurn of guiattack | NoPara of guiattack | Para
  | OHKill of guiattack | ChargingMove of string * string | FlinchA
  | Recoil of guiattack | PoisonMove of guiattack | Asleep | Wake of guiattack
  | Confused | BreakConfuse of guiattack | StatAttackA of stat * int * guiattack
  | ConfuseMoveA of guiattack | Recharging of guiattack | DrainA of guiattack
  | UserFaintA of guiattack | NoEffAll of string | DrainSleepFail of string
  | BreakSub of guiattack | SubDmg of guiattack | ProtectedA of string
  | StatBoostA of stat * int * guiattack | SwitchOutA of guiattack
  | FalseSwipeA of guiattack | ConfuseUserA of guiattack
  | KnockedOff of item * guiattack | SleepMove of guiattack
  | SleepAttack of guiattack | SleepAttackFail of string
  | TrappingMove of guiattack | NoRecoil of guiattack | LifeOrbA of guiattack
  | RapidSpinA of guiattack | HitSelf of guiattack | FailA of string
  | HealOppA of guiattack | MakeSleep of guiattack | LeechA of guiattack
  | BadPoisonMove of guiattack | HealHealth of guiattack | HazeA of guiattack
  | LightScreenA of guiattack | ReflectA of guiattack | RestA of guiattack
  | SubMake of guiattack | ProtectA of guiattack | ProtectFail of guiattack
  | SpikesA of guiattack | ToxicSpikesA of guiattack | HealBellA of guiattack
  | SunnyDayA of guiattack | RefreshA of guiattack | PsychUpA of guiattack
  | RainDanceA of guiattack | SandStormA of guiattack | HailA of guiattack
  | EncoreA of guiattack | EncoreFail | CopyPrevMoveA of guiattack
  | TauntFail | TauntA of guiattack | SleepTalkA of guiattack * guiattack
  | StealthRockA of guiattack
  | StickyWebA of guiattack | RandMoveA of guiattack | WishA of guiattack
  | AbilityChangeA of guiattack | GastroAcidA of guiattack
  | SubBlock of guiattack | SubFail of guiattack | ItemSwapA of guiattack
  | CopyFail | Taunted of string

type endMove = BurnDmg | BreakBurn | BreakFreeze  | BreakPara  | BreakPoison
              | PoisonDmg | LeechDmg of endMove | Base
              | LightScreenFade of endMove | ReflectFade of endMove
              | SunFade | RainFade
              | SandStormFade | SandBuffet
              | HailFade | HailBuffet
              | TauntFade of endMove | TrapDamage of string * endMove
              | LeftOversHeal | WishEnd of endMove
              | SpeedBoost | FaintpConvert | NoActionConvert | LoseGameConvert

type switchdescript = SwitchedInto of string | Intimidate of switchdescript
                      | StickyWebSlow of switchdescript
                      | SpikeDamage of switchdescript
                      | StealthRocksDamage of switchdescript
                      | ToxicSpikePoison of switchdescript
                      | MakeItRain of switchdescript
                      | MakeBlizzard of switchdescript
                      | MakeSunny of switchdescript
                      | MakeSandStorm of switchdescript
                      | SFaint of switchdescript

type guimove = SPoke of switchdescript | UsedMove of guiattack | Faintp
              | NoAction | EndMove of endMove | ForceChoose of guiattack
              | LoseGame

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
type secondary_effects =
   MultHit of int | StageBoost of (stat * int) list | IncCrit of int
   | RandMultHit | BurnChance | FreezeChance | ParaChance | OHKO
   | ChargeMove of string | ForceSwitch | FlinchMove
   | StageAttack of (stat * int) list | RecoilMove | PoisonChance | PutToSleep
   | ConfuseOpp | ConstantDmg of int | RechargeMove | WeightDamage | DrainMove
   | LeechSeed | ChargeInSunlight of string
   | ToxicChance | StageBoostSunlight of (stat * int) list | Recovery
   | LightScreenMake | Haze | ReflectMake | UserFaint | NeverMiss
   | DrainMoveSleep | VariableDamage | Rest | SuperFang | SubstituteMake | Flail
   | Protect | BellyDrum | Spikes | HealBell | SunHeal | MaxHealthDmg | SunnyDay
   | FalseSwipe | Refresh | PsychUp | RandStageBoost | FlowerShield | RainDance
   | SandStormMake | HailMake | Encore of int | PainSplit | SelfEncore
   | ConfuseUser | CopyPrevMove | KnockOff | ChanceStageBoost | SelfSwitch
   | FoulPlay | TauntMove | StealthRockMake | TSpikes | StickyWebMake
   | Rototiller | SleepEffect | BeatUp | CausePartialTrapping | DoublePower
   | StoredPower | VenomDrench | RandMove | ElectroBall | RapidSpin
   | ItemSwitch | WishMake | ChancePower | AbilityChange | ReverseStats
   | FinalGambit | GyroBall | PowerSwap | GuardSwap
   | HeartSwap | FakeOut | Facade | GastroAcid | SmellingSalts | PsychoShift
   | Endeavor | Counter

type move = {name:string; priority: int; target: target; dmg_class: dmg_class;
    mutable power:int; effect_chance: int; accuracy: int; element: element;
    description: string; secondary: secondary_effects list}

type non_volatile_status =
  Burn | Freeze | Paralysis | Poisoned | Toxic of int | Sleep of int | NoNon

type volatile_status =  Confusion of int | Flinch | Leeched
  | Charge | Substitute of int | Protected | UsedProtect
  | RechargingStatus | ForcedMove of int * string
  | ForcedMoveNoSwitch of int * string |
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

type battle_poke = {pokeinfo: pokemon; mutable curr_hp:int;
  mutable curr_status: status; mutable curr_item: item; bhp:int; battack:int;
  bdefense:int; bspecial_attack:int; bspecial_defense:int; bspeed:int;
  mutable curr_abil:string; mutable curr_type: element list}

(* stat modifier represents number of stages followed by multiplier *)
type stat_modifier = int * float

type pokemon_stat_modifier = {mutable attack: stat_modifier;
    mutable defense: stat_modifier;
    mutable speed: stat_modifier; mutable special_attack: stat_modifier;
    mutable special_defense: stat_modifier; mutable evasion: stat_modifier;
    mutable accuracy: stat_modifier}

type terrain_element = LightScreen of int | Reflect of int | Spikes of int
  | StealthRock | ToxicSpikes of int | StickyWeb | Wish of int*int
  | BatonPass of pokemon_stat_modifier * volatile_status list

type terrain =
  {side1: terrain_element list ref; side2: terrain_element list ref}

type weather = HarshSun of int | Hail of int | Rain of int | SandStorm of int
  | HeavyRain of int | Sun of int | AirCurrent of int | ClearSkies

type weather_terrain = {mutable weather: weather; terrain: terrain}

type trainer_team =
  {mutable current: battle_poke; mutable alive: battle_poke list; mutable dead:
                  battle_poke list; mutable stat_enhance: pokemon_stat_modifier}

(* As in competitive Pokemon, there will be no ties. If all Poke die in
one turn, the one with the Poke standing the latest wins *)
type outcome = WinnerP1 | WinnerP2

type battle_mode =
   Random1p | Random2p | Preset1p of pokemon list | TournBattle of pokemon list
   | Preset2p of pokemon list * pokemon list | Random0p

type screen = SwitchPoke | ChooseMove | Faint | BothFaint | SwitchPokeF

type battle_state =
  InGame of trainer_team * trainer_team * weather_terrain * playerMove ref
  | Loading | P1 of screen| P2 of screen | Processing

type game_state = MainMenu | Menu1P | Quit | Battle of battle_state | Menu2P
  | Menu0P | Preset1PChoose | Tourney | TourneyChoose | PokeEdit | PokeEditor
  | Preset2PChoose |  Preset2PChooseAgain of pokemon list

type preprocess = PreprocessWeather | PreprocessStatus | PreprocessItem | PreprocessAbility | PreprocessNonVola | PreprocessTerrain | ProcessNextTurn

type command = Player1 of battlemove | Player2 of battlemove | Buffer | Preprocess
                | Process1 of preprocess | Process2 of preprocess