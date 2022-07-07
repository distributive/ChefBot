# ChefBot

An extendable Discord bot framework written on top of [WarwickTabletop's Tablebot](https://github.com/WarwickTabletop/tablebot) (built on top of [discord-haskell](https://github.com/aquarial/discord-haskell)) and inspired by [Jeeves](https://github.com/Mezuzi/Jeeves) and [Beanstalk](https://github.com/dougblack/beanstalk). Note that a lot of commands were stripped from the original version of this bot, but the underlying code remains. If you find code that seems to be unused, it probably is.

## Functionality

Card finding:

![image](https://user-images.githubusercontent.com/26557961/160260122-5e30f950-bc22-42da-b19a-c7f5cd745678.png)

Displaying card art:

![image](https://user-images.githubusercontent.com/26557961/160260137-e8c79a0e-e730-477a-be71-9bd413b4f568.png)

Displaying card flavour text:

![image](https://user-images.githubusercontent.com/26557961/160260141-cdaf5a69-f51f-4a51-a588-eb8049705204.png)

Searching cards using NetrunnerDB syntax:

![image](https://user-images.githubusercontent.com/26557961/160260170-b0c9916e-6058-406f-b12d-24acf05589fc.png)

Displaying the banlist history of cards:

![image](https://user-images.githubusercontent.com/26557961/160260369-94eb4248-2429-4aaf-b0af-3fdf80af2921.png)

Other functionality:
- Displaying current and past banlists
- Displaying Netrunner rules and timing structures
- Showing random cards with optional constraints
- Horoscope

## Contributing

If you're interested in getting involved in contributing to this bot, and are completely new to git and/or Haskell, you might be interested in looking at the [Setup from Scratch](CONTRIBUTING.md#setup-from-scratch) section in the [contributor's guide](CONTRIBUTING.md). If you want tutorials on making your first plugin or how exceptions work, checkout the tutorials in the [tutorials](tutorials) folder.

## Environment file setup

Create a `.env` file containing the following keys. Consult `.env.example` if you're unsure how this should be formatted! Please note that the `.env` file must have a newline at the end of it - i.e. the last line should be blank.

- `DEBUG` (mandatory) - whether the bot should run in debug mode. This bypasses all permission checks, and prints
  certain log message that would otherwise be suppressed.
- `DISCORD_TOKEN` (mandatory) - the Discord token for your bot. Go to
  the [Discord Developer Portal](https://discord.com/developers/applications), create an application representing your
  bot, then create a bot user and copy its token.
- `PREFIX` (optional, defaults to `!`) - the prefix for each bot command. For example, if you set it to `$`, then you
  would call `$ping` to ping the bot.
- `SQLITE_FILENAME` (mandatory) - a name for your SQLite database, for example `database.db`.
- `MODERATOR_GROUP` (optional) - the group ID assigned to moderator members.
- `SUPERUSER_GROUP` (optional) - the group ID assigned to the superuser.
- `ALLOW_GIT_UPDATE` (mandatory) - whether superusers can update the bot from git through Discord.
- `EMOJI_SERVERS` (optional) - a list of server IDs that the bot will search for emoji within.

The two Group settings are optional, but without them any commands that require elevated permissions will not be able
to be called when `DEBUG` is false. Users with the superuser group are able to run every command (including some dangerous
ones), so caution should be used when setting these up.

If you have any difficulties setting it up, see the [contributor's guide](CONTRIBUTING.md) for a walkthrough.
