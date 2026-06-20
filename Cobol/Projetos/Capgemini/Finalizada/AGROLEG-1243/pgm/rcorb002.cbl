000010 IDENTIFICATION DIVISION.                                         RCORB002
000020 PROGRAM-ID. RCORB002.                                            RCORB002
000030*---------------------------------------------------------------*RCORB002
000040*                  B R A D E S C O   B B I                      *RCORB002
000050*---------------------------------------------------------------*RCORB002
000060* PROGRAMA......: RCORB002                                      *RCORB002
000070* DATA..........: 13/05/2026                                    *RCORB002
000080* PROGRAMADOR...: AGROLEG-1243                                  *RCORB002
000090* ANALISTA......: AGROLEG-1243                                  *RCORB002
000100*---------------------------------------------------------------*RCORB002
000110* OBJETIVO......: SIMULAR A COMPACTACAO DO WKTPART REALIZADA   *RCORB002
000120*                 PELO RCOR8015. LE A MENSAGEM GERADA PELO     *RCORB002
000130*                 RCORB001 E APLICA O MESMO ALGORITMO DE       *RCORB002
000140*                 COMPACTACAO. RELATA RESULTADO ANTES X DEPOIS.*RCORB002
000150*---------------------------------------------------------------*RCORB002
000160* ENTRADAS......: ARQSAIDA  - MENSAGEM COR0001E (RCORB001)    *RCORB002
000170* SAIDAS........: ARQSAIDA2 - MENSAGEM APOS COMPACTACAO       *RCORB002
000180*                 ARQRELAT  - RELATORIO ANTES X DEPOIS         *RCORB002
000190*---------------------------------------------------------------*RCORB002
000200 ENVIRONMENT DIVISION.                                            RCORB002
000210 CONFIGURATION SECTION.                                           RCORB002
000220 SOURCE-COMPUTER. IBM-370.                                        RCORB002
000230 OBJECT-COMPUTER. IBM-370.                                        RCORB002
000240 INPUT-OUTPUT SECTION.                                            RCORB002
000250 FILE-CONTROL.                                                    RCORB002
000260     SELECT ARQ-ENTRADA                                           RCORB002
000270         ASSIGN TO ARQSAIDA                                       RCORB002
000280         ORGANIZATION IS SEQUENTIAL                               RCORB002
000290         ACCESS IS SEQUENTIAL.                                    RCORB002
000300     SELECT ARQ-SAIDA2                                            RCORB002
000310         ASSIGN TO ARQSAIDA2                                      RCORB002
000320         ORGANIZATION IS SEQUENTIAL                               RCORB002
000330         ACCESS IS SEQUENTIAL.                                    RCORB002
000340     SELECT ARQ-RELATO                                            RCORB002
000350         ASSIGN TO ARQRELAT                                       RCORB002
000360         ORGANIZATION IS SEQUENTIAL                               RCORB002
000370         ACCESS IS SEQUENTIAL.                                    RCORB002
000380 DATA DIVISION.                                                   RCORB002
000390 FILE SECTION.                                                    RCORB002
000400 FD  ARQ-ENTRADA                                                  RCORB002
000410     RECORDING MODE IS F                                          RCORB002
000420     BLOCK CONTAINS 0 RECORDS                                     RCORB002
000430     RECORD CONTAINS 30000 CHARACTERS.                            RCORB002
000440 01  FS-ENTRADA                PIC X(30000).                      RCORB002
000450 FD  ARQ-SAIDA2                                                   RCORB002
000460     RECORDING MODE IS F                                          RCORB002
000470     BLOCK CONTAINS 0 RECORDS                                     RCORB002
000480     RECORD CONTAINS 30000 CHARACTERS.                            RCORB002
000490 01  FS-SAIDA2                 PIC X(30000).                      RCORB002
000500 FD  ARQ-RELATO                                                   RCORB002
000510     RECORDING MODE IS F                                          RCORB002
000520     BLOCK CONTAINS 0 RECORDS                                     RCORB002
000530     RECORD CONTAINS 0132 CHARACTERS.                             RCORB002
000540 01  FS-RELATO                 PIC X(0132).                       RCORB002
000550 WORKING-STORAGE SECTION.                                         RCORB002
000560*---------------------------------------------------------------*RCORB002
000570 01  FILLER                    PIC X(32)          VALUE            RCORB002
000580     '*  INICIO DA WORKING RCORB002  *'.                          RCORB002
000590*---------------------------------------------------------------*RCORB002
000600* AREA DE CONTROLE                                               *RCORB002
000610*---------------------------------------------------------------*RCORB002
000620 01  WRK-CONTROLE.                                                RCORB002
000630     05 WRK-STATUS             PIC X(001) VALUE SPACES.           RCORB002
000640     05 WRK-FIM-ARQ            PIC X(001) VALUE 'N'.              RCORB002
000650 01  WRK-LINHA                 PIC X(0132) VALUE SPACES.          RCORB002
000660 01  WRK-SEP                   PIC X(080) VALUE ALL '='.          RCORB002
000670 01  WRK-EDIT-6                PIC ZZZZZ9  VALUE ZEROS.           RCORB002
000680*---------------------------------------------------------------*RCORB002
000690* VARIAVEIS DE COMPACTACAO - IDENTICAS AO RCOR8015             *RCORB002
000700*---------------------------------------------------------------*RCORB002
000710 01  WRK-MSG-COR0001E          PIC X(30000) VALUE SPACES.         RCORB002
000720 01  IND-1                     PIC 9(05) COMP-3 VALUE ZEROS.      RCORB002
000730 01  WRK-EFF-LAST-MSG          PIC 9(05) COMP-3 VALUE ZEROS.      RCORB002
000740 01  WRK-WKT-SCAN-DES          PIC 9(05) COMP-3 VALUE ZEROS.      RCORB002
000750 01  WRK-WKT-I                 PIC 9(05) COMP-3 VALUE ZEROS.      RCORB002
000760 01  WRK-WKT-J                 PIC 9(05) COMP-3 VALUE ZEROS.      RCORB002
000770 01  WRK-WKT-POS-TAG2210       PIC 9(05) COMP-3 VALUE ZEROS.      RCORB002
000780 01  WRK-WKT-CSTART            PIC 9(05) COMP-3 VALUE ZEROS.      RCORB002
000790 01  WRK-WKT-HIT               PIC X(01) VALUE SPACES.            RCORB002
000800 01  WRK-WKT-CONTINUA          PIC X(01) VALUE SPACES.            RCORB002
000810 01  WRK-NEXT-BRA              PIC 9(05) COMP-3 VALUE ZEROS.      RCORB002
000820 01  WRK-WKT-IEND              PIC 9(05) COMP-3 VALUE ZEROS.      RCORB002
000830 01  WRK-WKT-OLD-LEN           PIC 9(05) COMP-3 VALUE ZEROS.      RCORB002
000840 01  WRK-WKT-NEW-LEN           PIC 9(05) COMP-3 VALUE ZEROS.      RCORB002
000850 01  WRK-WKT-DELTA             PIC 9(05) COMP-3 VALUE ZEROS.      RCORB002
000860 01  WRK-WKT-TAIL-L            PIC 9(05) COMP-3 VALUE ZEROS.      RCORB002
000870 01  WRK-WKT-OUT-IX            PIC 9(05) COMP-3 VALUE ZEROS.      RCORB002
000880 01  WRK-CH-WKT                PIC X(01) VALUE SPACES.            RCORB002
000890 01  WRK-WKT-PREV-SP           PIC X(01) VALUE SPACES.            RCORB002
000900 01  WRK-SP-REM                PIC 9(05) COMP-3 VALUE ZEROS.      RCORB002
000910 CG2605*01  WRK-WKT-BUF               PIC X(12000) VALUE SPACES.         RCORB002
000911 CG2605*01  WRK-WKT-BUF               PIC X(30000) VALUE SPACES.         RCORB002
000912 01  WRK-WKT-BUF               PIC X(10000) VALUE SPACES.         RCORB002
000920 01  WRK-OVERLAP-MOV           PIC X(30000) VALUE SPACES.         RCORB002
000930*---------------------------------------------------------------*RCORB002
000940* AREA DE RELATORIO                                              *RCORB002
000950*---------------------------------------------------------------*RCORB002
000960 01  WRK-RELAT.                                                    RCORB002
000970     05 WRK-TAM-ANTES          PIC 9(006) VALUE ZEROS.            RCORB002
000980     05 WRK-TAM-DEPOS          PIC 9(006) VALUE ZEROS.            RCORB002
000990     05 WRK-SP-REM-TOT         PIC 9(006) VALUE ZEROS.            RCORB002
001000     05 WRK-SNAP-CSTART        PIC 9(005) VALUE ZEROS.            RCORB002
001010     05 WRK-SNAP-ANTES         PIC X(080) VALUE SPACES.           RCORB002
001020     05 WRK-SNAP-DEPOS         PIC X(080) VALUE SPACES.           RCORB002
001030 PROCEDURE DIVISION.                                               RCORB002
001040*---------------------------------------------------------------*RCORB002
001050*---------------------------------------------------------------*RCORB002
001060  0000-PRINCIPAL               SECTION.                           RCORB002
001070*---------------------------------------------------------------*RCORB002
001080           PERFORM 1000-INICIALIZAR.                               RCORB002
001090           PERFORM 2000-LER-MENSAGEM.                              RCORB002
001100           IF  WRK-FIM-ARQ     NOT EQUAL 'S'                      RCORB002
001110               PERFORM 3000-CALCULA-TAMANHO                        RCORB002
001120               PERFORM 4000-NORMALIZA-WKTPART                      RCORB002
001130               PERFORM 5000-CAPTURA-SNAP-DEPOS                     RCORB002
001140               PERFORM 6000-GRAVAR-SAIDA                           RCORB002
001150               PERFORM 7000-IMPRIMIR-RELATORIO                     RCORB002
001160           END-IF.                                                 RCORB002
001170           PERFORM 9000-FINALIZAR.                                 RCORB002
001180           STOP RUN.                                               RCORB002
001190*---------------------------------------------------------------*RCORB002
001200  0000-99-FIM.                 EXIT.                              RCORB002
001210*---------------------------------------------------------------*RCORB002
001220*---------------------------------------------------------------*RCORB002
001230  1000-INICIALIZAR             SECTION.                           RCORB002
001240*---------------------------------------------------------------*RCORB002
001250           OPEN INPUT  ARQ-ENTRADA.                                RCORB002
001260           OPEN OUTPUT ARQ-SAIDA2.                                 RCORB002
001270           OPEN OUTPUT ARQ-RELATO.                                 RCORB002
001280*---------------------------------------------------------------*RCORB002
001290  1000-99-FIM.                 EXIT.                              RCORB002
001300*---------------------------------------------------------------*RCORB002
001310*---------------------------------------------------------------*RCORB002
001320  2000-LER-MENSAGEM            SECTION.                           RCORB002
001330*---------------------------------------------------------------*RCORB002
001340           READ ARQ-ENTRADA INTO WRK-MSG-COR0001E                 RCORB002
001350               AT END                                              RCORB002
001360                   MOVE 'S'   TO WRK-FIM-ARQ                      RCORB002
001370           END-READ.                                               RCORB002
001380*---------------------------------------------------------------*RCORB002
001390  2000-99-FIM.                 EXIT.                              RCORB002
001400*---------------------------------------------------------------*RCORB002
001410*---------------------------------------------------------------*RCORB002
001420  3000-CALCULA-TAMANHO         SECTION.                           RCORB002
001430*AG1243 DETERMINA TAMANHO EFETIVO E CAPTURA SNAP WKT ANTES     *RCORB002
001440*---------------------------------------------------------------*RCORB002
001450           MOVE 30000             TO WRK-WKT-I.                   RCORB002
001460           PERFORM VARYING WRK-WKT-I FROM 30000 BY -1             RCORB002
001470               UNTIL WRK-WKT-I   EQUAL ZEROS                      RCORB002
001480                  OR WRK-MSG-COR0001E(WRK-WKT-I:1)                RCORB002
001490                     NOT EQUAL SPACES                              RCORB002
001500           END-PERFORM.                                            RCORB002
001510           MOVE WRK-WKT-I         TO IND-1.                       RCORB002
001520           MOVE IND-1             TO WRK-TAM-ANTES.               RCORB002
001530*--- LOCALIZA POSICAO DO PRIMEIRO WKTPART PARA SNAP            *RCORB002
001540           MOVE ZEROS             TO WRK-WKT-I.                   RCORB002
001550           PERFORM VARYING WRK-WKT-I FROM 1 BY 1                  RCORB002
001560               UNTIL WRK-WKT-I   GREATER THAN IND-1               RCORB002
001570                  OR WRK-MSG-COR0001E(WRK-WKT-I:6)                RCORB002
001580                     EQUAL '{2210}'                                RCORB002
001590           END-PERFORM.                                            RCORB002
001600           IF  WRK-WKT-I         NOT GREATER THAN IND-1           RCORB002
001610               COMPUTE WRK-SNAP-CSTART = WRK-WKT-I + 6           RCORB002
001620               MOVE WRK-MSG-COR0001E                               RCORB002
001630                    (WRK-SNAP-CSTART:80)                           RCORB002
001640                                    TO WRK-SNAP-ANTES              RCORB002
001650           END-IF.                                                 RCORB002
001660*---------------------------------------------------------------*RCORB002
001670  3000-99-FIM.                 EXIT.                              RCORB002
001680*---------------------------------------------------------------*RCORB002
001690*---------------------------------------------------------------*RCORB002
001700  4000-NORMALIZA-WKTPART       SECTION.                           RCORB002
001710*AG1243 LOGICA IDENTICA A 1050-NORMALIZA-WKT-WKTPART (RCOR8015)*RCORB002
001720*---------------------------------------------------------------*RCORB002
001730           MOVE IND-1             TO WRK-EFF-LAST-MSG.            RCORB002
001740           IF  WRK-EFF-LAST-MSG   EQUAL ZEROS                     RCORB002
001750            OR WRK-EFF-LAST-MSG   LESS THAN +7                    RCORB002
001760            OR WRK-EFF-LAST-MSG   GREATER THAN +29994             RCORB002
001770               GO                 TO 4000-99-FIM.                 RCORB002
001780           MOVE +1                TO WRK-WKT-SCAN-DES.            RCORB002
001790           MOVE 'S'               TO WRK-WKT-CONTINUA.            RCORB002
001800           PERFORM 4010-WKT-BUSCA-TAG                              RCORB002
001810               UNTIL WRK-WKT-CONTINUA NOT EQUAL 'S'.              RCORB002
001820           MOVE WRK-EFF-LAST-MSG  TO IND-1.                       RCORB002
001830*---------------------------------------------------------------*RCORB002
001840  4000-99-FIM.                 EXIT.                              RCORB002
001850*---------------------------------------------------------------*RCORB002
001860*---------------------------------------------------------------*RCORB002
001870  4010-WKT-BUSCA-TAG           SECTION.                           RCORB002
001880*AG1243 LOGICA IDENTICA A 1051-WKT-BUSCA-TAG (RCOR8015)        *RCORB002
001890*---------------------------------------------------------------*RCORB002
001900           MOVE 'N'              TO WRK-WKT-HIT.                  RCORB002
001910           PERFORM VARYING WRK-WKT-I                              RCORB002
001920                         FROM WRK-WKT-SCAN-DES BY +1              RCORB002
001930                         UNTIL WRK-WKT-I  GREATER THAN            RCORB002
001940                               WRK-EFF-LAST-MSG                   RCORB002
001950                            OR WRK-WKT-HIT EQUAL 'S'              RCORB002
001960               IF  WRK-MSG-COR0001E(WRK-WKT-I:6)                 RCORB002
001970                                    EQUAL '{2210}'                 RCORB002
001980                   MOVE 'S'      TO WRK-WKT-HIT                   RCORB002
001990                   MOVE WRK-WKT-I                                  RCORB002
002000                        TO WRK-WKT-POS-TAG2210                    RCORB002
002010                   COMPUTE WRK-WKT-CSTART = WRK-WKT-I + 6        RCORB002
002020               END-IF                                              RCORB002
002030           END-PERFORM.                                            RCORB002
002040           IF  WRK-WKT-HIT       EQUAL 'N'                        RCORB002
002050               MOVE 'N'          TO WRK-WKT-CONTINUA              RCORB002
002060               GO                TO 4010-99-FIM                   RCORB002
002070           END-IF.                                                 RCORB002
002080           PERFORM 4020-WKT-LOCALIZA-FIM.                         RCORB002
002090*---------------------------------------------------------------*RCORB002
002100  4010-99-FIM.                 EXIT.                              RCORB002
002110*---------------------------------------------------------------*RCORB002
002120*---------------------------------------------------------------*RCORB002
002130  4020-WKT-LOCALIZA-FIM        SECTION.                           RCORB002
002140*AG1243 LOGICA IDENTICA A 1052-WKT-LOCALIZA-FIM (RCOR8015)     *RCORB002
002150*---------------------------------------------------------------*RCORB002
002160           MOVE 'N'              TO WRK-WKT-HIT.                  RCORB002
002170           MOVE ZEROS             TO WRK-NEXT-BRA.                RCORB002
002180           PERFORM VARYING WRK-WKT-J                              RCORB002
002190                         FROM WRK-WKT-CSTART BY +1                RCORB002
002200                         UNTIL WRK-WKT-J  GREATER THAN            RCORB002
002210                               WRK-EFF-LAST-MSG                   RCORB002
002220                            OR WRK-WKT-HIT EQUAL 'S'              RCORB002
002230               IF  WRK-MSG-COR0001E(WRK-WKT-J:1)                 RCORB002
002240                                    EQUAL '{'                      RCORB002
002250                   MOVE 'S'      TO WRK-WKT-HIT                   RCORB002
002260                   MOVE WRK-WKT-J TO WRK-NEXT-BRA                 RCORB002
002270               END-IF                                              RCORB002
002280           END-PERFORM.                                            RCORB002
002290           IF  WRK-NEXT-BRA      EQUAL ZEROS                      RCORB002
002300               COMPUTE WRK-NEXT-BRA = WRK-EFF-LAST-MSG + 1       RCORB002
002310           END-IF.                                                 RCORB002
002320           COMPUTE WRK-WKT-IEND  = WRK-NEXT-BRA - 1.             RCORB002
002330           COMPUTE WRK-WKT-OLD-LEN = WRK-WKT-IEND                RCORB002
002340                                   - WRK-WKT-CSTART + 1.         RCORB002
002350           PERFORM 4030-WKT-COMPACTAR-SEGDO.                      RCORB002
002360           IF  WRK-WKT-DELTA     GREATER THAN ZEROS               RCORB002
002370               COMPUTE WRK-WKT-TAIL-L = WRK-EFF-LAST-MSG         RCORB002
002380                                       - WRK-WKT-IEND             RCORB002
002390               IF  WRK-WKT-TAIL-L GREATER THAN ZEROS              RCORB002
002400                   MOVE WRK-MSG-COR0001E                           RCORB002
002410                        (WRK-NEXT-BRA:WRK-WKT-TAIL-L)             RCORB002
002420                                        TO WRK-OVERLAP-MOV        RCORB002
002430                   COMPUTE WRK-WKT-OUT-IX = WRK-WKT-CSTART       RCORB002
002440                                          + WRK-WKT-NEW-LEN       RCORB002
002450                   MOVE WRK-OVERLAP-MOV(1:WRK-WKT-TAIL-L)        RCORB002
002460                        TO WRK-MSG-COR0001E                       RCORB002
002470                           (WRK-WKT-OUT-IX:WRK-WKT-TAIL-L)       RCORB002
002480               END-IF                                              RCORB002
002490               COMPUTE WRK-EFF-LAST-MSG = WRK-EFF-LAST-MSG        RCORB002
002500                                        - WRK-WKT-DELTA           RCORB002
002510               COMPUTE WRK-WKT-OUT-IX   = WRK-EFF-LAST-MSG + 1   RCORB002
002520               MOVE SPACES              TO WRK-MSG-COR0001E        RCORB002
002530                                           (WRK-WKT-OUT-IX:       RCORB002
002540                                            WRK-WKT-DELTA)        RCORB002
002550           END-IF.                                                 RCORB002
002560           COMPUTE WRK-WKT-SCAN-DES = WRK-WKT-CSTART             RCORB002
002570                                    + WRK-WKT-NEW-LEN.            RCORB002
002580*---------------------------------------------------------------*RCORB002
002590  4020-99-FIM.                 EXIT.                              RCORB002
002600*---------------------------------------------------------------*RCORB002
002610*---------------------------------------------------------------*RCORB002
002620  4030-WKT-COMPACTAR-SEGDO     SECTION.                           RCORB002
002630*AG1243 LOGICA IDENTICA A 1053-WKT-COMPACTAR-SEGDO (RCOR8015)  *RCORB002
002640*---------------------------------------------------------------*RCORB002
002650           IF  WRK-WKT-OLD-LEN   EQUAL ZEROS                      RCORB002
CG2605*           OR WRK-WKT-OLD-LEN   GREATER THAN 12000               RCORB002
CG2605*           OR WRK-WKT-OLD-LEN   GREATER THAN 30000               RCORB002
002660            OR WRK-WKT-OLD-LEN   GREATER THAN 10000               RCORB002
002670               MOVE WRK-WKT-OLD-LEN TO WRK-WKT-NEW-LEN           RCORB002
002680               MOVE ZEROS           TO WRK-WKT-DELTA              RCORB002
002690               GO                   TO 4030-99-FIM.               RCORB002
002700           MOVE ZEROS             TO WRK-WKT-NEW-LEN.             RCORB002
002710           MOVE ZEROS             TO WRK-SP-REM.                  RCORB002
002720           MOVE SPACES            TO WRK-WKT-PREV-SP.             RCORB002
002730           MOVE SPACES            TO WRK-WKT-BUF.                 RCORB002
002740           PERFORM VARYING WRK-WKT-OUT-IX                         RCORB002
002750                         FROM WRK-WKT-CSTART BY +1                RCORB002
002760                         UNTIL WRK-WKT-OUT-IX GREATER THAN        RCORB002
002770                               WRK-WKT-IEND                       RCORB002
002780               MOVE WRK-MSG-COR0001E(WRK-WKT-OUT-IX:1)           RCORB002
002790                                       TO WRK-CH-WKT               RCORB002
002800               IF  WRK-CH-WKT        EQUAL SPACES                 RCORB002
002810               AND WRK-WKT-PREV-SP   EQUAL SPACES                 RCORB002
002820                   ADD +1             TO WRK-SP-REM                RCORB002
002830               ELSE                                                RCORB002
002840                   ADD +1             TO WRK-WKT-NEW-LEN          RCORB002
002850                   MOVE WRK-CH-WKT                                 RCORB002
002860                        TO WRK-WKT-BUF(WRK-WKT-NEW-LEN:1)        RCORB002
002870               END-IF                                              RCORB002
002880               MOVE WRK-CH-WKT       TO WRK-WKT-PREV-SP           RCORB002
002890           END-PERFORM.                                            RCORB002
002900           IF  WRK-WKT-NEW-LEN    GREATER THAN ZEROS              RCORB002
002910               MOVE WRK-WKT-BUF(1:WRK-WKT-NEW-LEN)               RCORB002
002920                    TO WRK-MSG-COR0001E                            RCORB002
002930                       (WRK-WKT-CSTART:WRK-WKT-NEW-LEN)          RCORB002
002940           END-IF.                                                 RCORB002
002950           COMPUTE WRK-WKT-DELTA = WRK-WKT-OLD-LEN                RCORB002
002960                                 - WRK-WKT-NEW-LEN.               RCORB002
002970*---------------------------------------------------------------*RCORB002
002980  4030-99-FIM.                 EXIT.                              RCORB002
002990*---------------------------------------------------------------*RCORB002
003000*---------------------------------------------------------------*RCORB002
003010  5000-CAPTURA-SNAP-DEPOS      SECTION.                           RCORB002
003020*AG1243 CAPTURA PRIMEIROS 80 CHARS DO WKT DA GLEBA 1 APOS COMP *RCORB002
003030*---------------------------------------------------------------*RCORB002
003040           MOVE IND-1             TO WRK-TAM-DEPOS.               RCORB002
003050           COMPUTE WRK-SP-REM-TOT = WRK-TAM-ANTES                 RCORB002
003060                                  - WRK-TAM-DEPOS.                RCORB002
003070           IF  WRK-SNAP-CSTART    GREATER THAN ZEROS              RCORB002
003080           AND WRK-SNAP-CSTART    LESS THAN IND-1                 RCORB002
003090               MOVE WRK-MSG-COR0001E                               RCORB002
003100                    (WRK-SNAP-CSTART:80)                           RCORB002
003110                                    TO WRK-SNAP-DEPOS              RCORB002
003120           END-IF.                                                 RCORB002
003130*---------------------------------------------------------------*RCORB002
003140  5000-99-FIM.                 EXIT.                              RCORB002
003150*---------------------------------------------------------------*RCORB002
003160*---------------------------------------------------------------*RCORB002
003170  6000-GRAVAR-SAIDA            SECTION.                           RCORB002
003180*---------------------------------------------------------------*RCORB002
003190           MOVE WRK-MSG-COR0001E  TO FS-SAIDA2.                   RCORB002
003200           WRITE FS-SAIDA2.                                        RCORB002
003210*---------------------------------------------------------------*RCORB002
003220  6000-99-FIM.                 EXIT.                              RCORB002
003230*---------------------------------------------------------------*RCORB002
003240*---------------------------------------------------------------*RCORB002
003250  7000-IMPRIMIR-RELATORIO      SECTION.                           RCORB002
003260*---------------------------------------------------------------*RCORB002
003270           MOVE SPACES            TO WRK-LINHA.                   RCORB002
003280           MOVE WRK-SEP           TO WRK-LINHA(001:080).          RCORB002
003290           WRITE FS-RELATO FROM WRK-LINHA.                        RCORB002
003300           MOVE SPACES            TO WRK-LINHA.                   RCORB002
003310           MOVE 'RCORB002 - SIMULACAO COMPACTACAO WKTPART'        RCORB002
003320                                  TO WRK-LINHA.                   RCORB002
003330           WRITE FS-RELATO FROM WRK-LINHA.                        RCORB002
003340           MOVE SPACES            TO WRK-LINHA.                   RCORB002
003350           MOVE WRK-SEP           TO WRK-LINHA(001:080).          RCORB002
003360           WRITE FS-RELATO FROM WRK-LINHA.                        RCORB002
003370           MOVE SPACES            TO WRK-LINHA.                   RCORB002
003380           WRITE FS-RELATO FROM WRK-LINHA.                        RCORB002
003390*--- TAMANHO ANTES                                                RCORB002
003400           MOVE SPACES            TO WRK-LINHA.                   RCORB002
003410           MOVE WRK-TAM-ANTES     TO WRK-EDIT-6.                  RCORB002
003420           MOVE 'TAMANHO MSG ANTES'                                RCORB002
003430                                  TO WRK-LINHA(001:017).          RCORB002
003440           MOVE WRK-EDIT-6        TO WRK-LINHA(021:006).          RCORB002
003450           MOVE 'BYTES'           TO WRK-LINHA(028:005).          RCORB002
003460           WRITE FS-RELATO FROM WRK-LINHA.                        RCORB002
003470*--- TAMANHO DEPOIS                                               RCORB002
003480           MOVE SPACES            TO WRK-LINHA.                   RCORB002
003490           MOVE WRK-TAM-DEPOS     TO WRK-EDIT-6.                  RCORB002
003500           MOVE 'TAMANHO MSG APOS COMPACT'                         RCORB002
003510                                  TO WRK-LINHA(001:024).          RCORB002
003520           MOVE WRK-EDIT-6        TO WRK-LINHA(026:006).          RCORB002
003530           MOVE 'BYTES'           TO WRK-LINHA(033:005).          RCORB002
003540           WRITE FS-RELATO FROM WRK-LINHA.                        RCORB002
003550*--- ESPACOS REMOVIDOS                                            RCORB002
003560           MOVE SPACES            TO WRK-LINHA.                   RCORB002
003570           MOVE WRK-SP-REM-TOT    TO WRK-EDIT-6.                  RCORB002
003580           MOVE 'ESPACOS REMOVIDOS'                                RCORB002
003590                                  TO WRK-LINHA(001:017).          RCORB002
003600           MOVE WRK-EDIT-6        TO WRK-LINHA(021:006).          RCORB002
003610           MOVE 'BYTES'           TO WRK-LINHA(028:005).          RCORB002
003620           WRITE FS-RELATO FROM WRK-LINHA.                        RCORB002
003630           MOVE SPACES            TO WRK-LINHA.                   RCORB002
003640           WRITE FS-RELATO FROM WRK-LINHA.                        RCORB002
003650*--- SNAP WKT ANTES                                               RCORB002
003660           MOVE SPACES            TO WRK-LINHA.                   RCORB002
003670           MOVE 'WKT GLEBA-1 ANTES (80 CHARS):'                   RCORB002
003680                                  TO WRK-LINHA(001:030).          RCORB002
003690           WRITE FS-RELATO FROM WRK-LINHA.                        RCORB002
003700           MOVE SPACES            TO WRK-LINHA.                   RCORB002
003710           MOVE WRK-SNAP-ANTES    TO WRK-LINHA(001:080).          RCORB002
003720           WRITE FS-RELATO FROM WRK-LINHA.                        RCORB002
003730           MOVE SPACES            TO WRK-LINHA.                   RCORB002
003740           WRITE FS-RELATO FROM WRK-LINHA.                        RCORB002
003750*--- SNAP WKT APOS COMPACTACAO                                    RCORB002
003760           MOVE SPACES            TO WRK-LINHA.                   RCORB002
003770           MOVE 'WKT GLEBA-1 APOS COMPACT (80 CHARS):'            RCORB002
003780                                  TO WRK-LINHA(001:038).          RCORB002
003790           WRITE FS-RELATO FROM WRK-LINHA.                        RCORB002
003800           MOVE SPACES            TO WRK-LINHA.                   RCORB002
003810           MOVE WRK-SNAP-DEPOS    TO WRK-LINHA(001:080).          RCORB002
003820           WRITE FS-RELATO FROM WRK-LINHA.                        RCORB002
003830           MOVE SPACES            TO WRK-LINHA.                   RCORB002
003840           WRITE FS-RELATO FROM WRK-LINHA.                        RCORB002
003850           MOVE SPACES            TO WRK-LINHA.                   RCORB002
003860           MOVE WRK-SEP           TO WRK-LINHA(001:080).          RCORB002
003870           WRITE FS-RELATO FROM WRK-LINHA.                        RCORB002
003880*---------------------------------------------------------------*RCORB002
003890  7000-99-FIM.                 EXIT.                              RCORB002
003900*---------------------------------------------------------------*RCORB002
003910*---------------------------------------------------------------*RCORB002
003920  9000-FINALIZAR               SECTION.                           RCORB002
003930*---------------------------------------------------------------*RCORB002
003940           CLOSE ARQ-ENTRADA.                                      RCORB002
003950           CLOSE ARQ-SAIDA2.                                       RCORB002
003960           CLOSE ARQ-RELATO.                                       RCORB002
003970*---------------------------------------------------------------*RCORB002
003980  9000-99-FIM.                 EXIT.                              RCORB002
003990*---------------------------------------------------------------*RCORB002
