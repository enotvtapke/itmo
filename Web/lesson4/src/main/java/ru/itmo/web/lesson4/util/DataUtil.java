package ru.itmo.web.lesson4.util;

import ru.itmo.web.lesson4.model.Post;
import ru.itmo.web.lesson4.model.User;

import javax.servlet.http.HttpServletRequest;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

public class DataUtil {
    private static final List<User> USERS = Arrays.asList(
            new User(1, "MikeMirzayanov", "Mike Mirzayanov", User.Color.RED),
            new User(6, "pashka", "Pavel Mavrin", User.Color.GREEN),
            new User(9, "geranazarov555", "Georgiy Nazarov", User.Color.GREEN),
            new User(11, "tourist", "Gennady Korotkevich", User.Color.BLUE),
            new User(12, "MuadDib", "Paul Atreides", User.Color.RED),
            new User(20, "Envoy", "Takeshi Kovacs", User.Color.PURPLE)
    );

    private static final List<Post> POSTS = Arrays.asList(
            new Post(1,
                    "Codeforces Round #510 (Div. 2)",
                    "Hello, Codeforces\n" +
                        "Codeforces Round #510 (Div. 2) will start at Monday, September 17, 2018 at 11:05. The round will be rated for Div. 2 contestants (participants with the rating below 2100). Div. 1 participants can take a part out of competition as usual.\n" +
                        "This round is held on the tasks of the school stage All-Russian Olympiad of Informatics 2018/2019 year in city Saratov. The problems were prepared by PikMike, fcspartakm, Ne0n25, BledDest, Ajosteen and Vovuh. Great thanks to our coordinator _kun_ for the help with the round preparation! I also would like to thank our testers DavidDenisov, PrianishnikovaRina, Decibit and Vshining.\n" +
                        "UPD: The scoring distribution is 500-1000-1500-2000-2250-2750.",
                    6),
            new Post(4,
                    "Understanding and analysis of B+ trees on NVM",
                    "The emerging non-volatile memory (NVM) possesses DRAM-like performance and disk-like persistency, driving a trend of building single-level storage systems by replacing DRAM and disks. Using NVM as the universal main memory brings opportunities and challenges to the design of new persistent in-memory data structures. In this context, several prior works have designed consistent and persistent B+ trees on NVM. However, All of them evaluate performance of B+ trees by applying an NVM performance simulator and can not provide concrete guidance on how to develop B+ trees with good performance on NVM. In this paper, by using Optane DCs, we aim to study and analyze the influence factors of designing B+ trees on NVM through a series of experiments and provide guidance on how to design efficient B+ trees on NVM. According to our experiments and analysis, we draw several conclusions which are either not presented in prior works, or contrary to current ideas. We discover that the performance of B+ trees is greatly affected by data formats. For example, we analyze the software layer optimizations and hardware layer optimizations separately and find that software layer optimizations do not always improve performance. Furthermore, B+ trees place multiple entries on one node and the shift and balance overhead of FPTree accounts for 39% of the total overhead.",
                    1),
            new Post(7,
                    "Altered carbon",
                    "Two hours before dawn I sat in the peeling kitchen and smoked one of Sarah’s cigarettes, listening to the maelstrom and waiting. Millsport had long since put itself to bed, but out in the Reach currents were still snagging on the shoals, and the sound came ashore to prowl the empty streets. There was a fine mist drifting in from the whirlpool, falling on the city like sheets of muslin and fogging the kitchen windows.\n" +
                            "\n" +
                            "  Chemically alert, I inventoried the hardware on the scarred wooden table for the fiftieth time that night. Sarah’s Heckler and Koch shard pistol glinted dully at me in the low light, the butt gaping open for its clip. It was an assassin’s weapon, compact and utterly silent. The magazines lay next to it. She had wrapped insulating tape around each one to distinguish the ammunition: green for sleep, black for the spider-venom load. Most of the clips were black-wrapped. Sarah had used up a lot of green on the security guards at Gemini Biosys last night.\n" +
                            "\n" +
                            "  My own contributions were less subtle: the big silver Smith & Wesson, and the four remaining hallucinogen grenades. The thin crimson line around each canister seemed to sparkle slightly, as if it was about to detach itself from the metal casing and float up to join the curlicues of smoke ribboning off my cigarette. Shift and slide of altered significants, the side effect of the tetrameth I’d scored that afternoon down at the wharf. I don’t usually smoke when I’m straight, but for some reason the tet always triggers the urge.",
                    20)
    );

    public static void addData(HttpServletRequest request, Map<String, Object> data) {
        data.put("users", USERS);

        for (User user : USERS) {
            if (Long.toString(user.getId()).equals(request.getParameter("logged_user_id"))) {
                data.put("user", user);
            }
        }

        data.put("posts", POSTS);
    }
}
