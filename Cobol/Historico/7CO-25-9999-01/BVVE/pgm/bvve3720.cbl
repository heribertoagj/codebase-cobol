000010***** Convertido de OS/VS COBOL p/ COBOL for MVS em 26/08/02 12:51
000020 ID  DIVISION.
000030 PROGRAM-ID.    BVVE3720.
000040*REMARKS.
000050*    ***********************************************
000060*    *                                             *
000070*    * ANALISTA             - EDU  -  GP.34.       *
000080*    * DATA                 - 16/12/92             *
000090*    *                                             *
000100*    *---------------------------------------------*
000110*    *                                             *
000120*    *                  OBJETIVO                   *
000130*    *  GERAR ATRAVES DO ARQUIVO DE CARGA DOS DB'S *
000140*    *  DO KELO, ARQUIVO PARA CARGA/EMIS/ALTER NO  *
000150*    *  CREDITMASTER VERSAO 3.2.                   *
000160*    *                                             *
000170*    *  INCLUSAO ALTERACAO DE   V E N D E D O R    *
000180*    *  NO CREDITMASTER 3.2  E  1.1                *
000190*    *                                             *
000210*    *                                             *
000220*    ***********************************************
000230*
000240*    *************** ULTIMA ALTERACAO **************
000250*    *                                             *
000260*    * ANALISTA             - IVONEIDE             *
000270*    * SUPERVISOR           - PEDRO                *
000280*    * PROGRAMADORA         - ELIANE  - CPM        *
000290*    * DATA                 - 01/08/94             *
000300*    *                                             *
000310*    *---------------------------------------------*
000320*    *                                             *
000330*    *                  OBJETIVO                   *
000340*    *                                             *
000350*    *     TRATAR CEP COM 8 POSICOES               *
000360*    *                                             *
000370*    ***********************************************
000380*    *************** ULTIMA ALTERACAO **************
000390*    *                                             *
000400*    * ANALISTA             - SOREIA  - GR 90      *
000420*    * PROGRAMADORA         - CIDALIA - CPM        *
000430*    * DATA                 - 17/03/97             *
000440**   *                                             *
000450*    *---------------------------------------------*
000460*    *                                             *
000470*    *                  OBJETIVO                   *
000480*    *                                             *
000490*    *    ALTERAR CENTRO DE CUSTO DE BACC PARA BVVE*
000500*    *                                             *
000510****************************************************
000520****************************************************************
000530*    U  L  T  I  M  A    A  L  T  E  R  A  C  A  O   :         *
000540****************************************************************
000550*                                                              *
000560* PROGRAMADOR...: ROMULO MARCHINA SOARES - CPM                 *
000570* ANALISTA......: VALERIA                - CPM                 *
000580* DATA..........: 07/10/1998                                   *
000590*                                                              *
000600* OBJETIVO......: CONVERSAO DE TODAS AS DATAS CONTEMPLANDO A   *
000610*                 VIRADA DO SECULO.                            *
000630****************************************************************
000640/
000650 ENVIRONMENT DIVISION.
000660 CONFIGURATION SECTION.
000670 SPECIAL-NAMES.
000680     DECIMAL-POINT IS COMMA.
000690 INPUT-OUTPUT SECTION.
000700 FILE-CONTROL.
000710
000720     SELECT  CARGATUA   ASSIGN  TO  UT-S-CARGATUA.
000730     SELECT  CMASTE32   ASSIGN  TO  UT-S-CMASTE32.
000740 DATA DIVISION.
000750 FILE SECTION.
000760/
000770*---------------------------------------------------------*
000780*              CARGATUA    -    INPUT                     *
000790*                    LRECL - 234                          *
000800*---------------------------------------------------------*
000810
000820 FD  CARGATUA
000840     LABEL RECORD STANDARD
000850     RECORDING F.
000860
000870 01  REG-CARG.
000880     02  CARG-PREFIXO               PIC S9(5)      COMP-3.
000890     02  CARG-NUMCART               PIC S9(9)      COMP-3.
000900     02  CARG-BANCO                 PIC S9(3)      COMP-3.
000910     02  CARG-AGENCIA               PIC S9(5)      COMP-3.
000920     02  CARG-CONTA                 PIC S9(7)      COMP-3.
000930     02  CARG-ASTER                 PIC X(1).
000940     02  CARG-RAZAO                 PIC S9(5)      COMP-3.
000950     02  CARG-CGC.
000960         03  CARG-CGC-PRINC         PIC S9(9)      COMP-3.
000970         03  CARG-CGC-FILIA         PIC S9(5)      COMP-3.
000980         03  CARG-CGC-CONTR         PIC S9(3)      COMP-3.
000990     02  CARG-RAZAO-COML            PIC X(40).
001000     02  CARG-DT-CONVENIO           PIC S9(9)      COMP-3.
001010     02  CARG-NR-MAQUINAS           PIC S9(5)      COMP-3.
001020     02  CARG-CD-ATIVIDADE          PIC S9(5)      COMP-3.
001030     02  CARG-DT-ULT-ATUAL          PIC S9(9)      COMP-3.
001050     02  CARG-TT-VIS-ATU            PIC 9(13)      COMP-3.
001060     02  CARG-TT-PZO-ANT            PIC 9(13)      COMP-3.
001070     02  CARG-TT-PZO-ATU            PIC 9(13)      COMP-3.
001080     02  CARG-FILLER                PIC X(01).
001090     02  CARG-NOME-VEND             PIC X(35).
001100     02  CARG-ENDER-VEND            PIC X(35).
001110     02  CARG-CEP                   PIC S9(5)      COMP-3.
001120     02  CARG-TP-CRED               PIC X(1).
001130     02  CARG-DIAS                  PIC X(2).
001140     02  CARG-LIM-OPER              PIC S9(9)      COMP-3.
001150     02  CARG-DT-CADASTR            PIC S9(7)      COMP-3.
001160     02  CARG-FONE                  PIC S9(7)      COMP-3.
001170     02  CARG-TAXAS                 PIC X(12).
001180     02  CARG-SFCEP                 PIC X(3).
001190     02  CARG-RESERVA               PIC X(3).
001200     02  CARG-COD-ATUAL             PIC X(1).
001210     02  FILLER                     PIC X(10).
001220/
001230*---------------------------------------------------------*
001240*              CMASTER32   -   OUTPUT                     *
001260*---------------------------------------------------------*
001270 FD  CMASTE32
001280     BLOCK 0
001290     LABEL RECORD STANDARD
001300     RECORDING F.
001310
001320 01  REG-CMAS.
001330     02  CMAS-NUMCART               PIC 9(9)       COMP-3.
001340     02  CMAS-BANCO                 PIC 9(3)       COMP-3.
001350     02  CMAS-AGENCIA               PIC 9(5)       COMP-3.
001360     02  CMAS-RAZAO                 PIC 9(5)       COMP-3.
001370     02  CMAS-CONTA                 PIC 9(7)       COMP-3.
001380     02  CMAS-CGC.
7C2511*        03  CMAS-CGC-PRINC         PIC 9(9)       COMP-3.
7C2511         03  CMAS-CGC-PRINC-ST      PIC X(09).
7C2511*        03  CMAS-CGC-FILIA         PIC 9(5)       COMP-3.
7C2511         03  CMAS-CGC-FILIA-ST      PIC X(04).
001410         03  CMAS-CGC-CONTR         PIC 9(3)       COMP-3.
001420     02  CMAS-DT-CADASTR            PIC 9(7)       COMP-3.
001430     02  CMAS-DT-CONVENIO           PIC 9(9)       COMP-3.
001440     02  CMAS-DT-ULT-ATUAL          PIC 9(9)       COMP-3.
001450     02  CMAS-NR-MAQUINAS           PIC 9(5)       COMP-3.
001470     02  CMAS-LIM-OPER              PIC 9(9)       COMP-3.
001480     02  CMAS-TP-CRED               PIC X(1).
001490     02  CMAS-DIAS                  PIC X(2).
001500     02  CMAS-NOME-VEND             PIC X(35).
001510     02  CMAS-ENDER-VEND            PIC X(35).
001520     02  CMAS-CIDAD-VEND            PIC X(20).
001530     02  CMAS-CEP                   PIC 9(5).
001540     02  CMAS-CEP-SEQ               PIC X(3).
001550     02  CMAS-UF                    PIC X(2).
001560     02  CMAS-RAZAO-COML            PIC X(40).
001570     02  CMAS-FONE                  PIC 9(7)       COMP-3.
001580     02  CMAS-TAXAS                 PIC X(12).
001590     02  CMAS-RESERVA               PIC X(09).
001600/
001610 WORKING-STORAGE SECTION.
001620/
001630 PROCEDURE DIVISION.
001640
001650     OPEN INPUT  CARGATUA
001660          OUTPUT CMASTE32.
001680 LER-CARGATUA.
001690
001700     READ CARGATUA AT END GO TO FIM-CARG.
001710
001720     IF CARG-PREFIXO  EQUAL  4560
001730        MOVE CARG-NUMCART      TO CMAS-NUMCART
001740        MOVE CARG-BANCO        TO CMAS-BANCO
001750        MOVE CARG-AGENCIA      TO CMAS-AGENCIA
001760        MOVE CARG-RAZAO        TO CMAS-RAZAO
001770        MOVE CARG-CONTA        TO CMAS-CONTA
7C2511*       MOVE CARG-CGC-PRINC    TO CMAS-CGC-PRINC
7C2511        MOVE CARG-CGC-PRINC    TO CMAS-CGC-PRINC-ST
7C2511*       MOVE CARG-CGC-FILIA    TO CMAS-CGC-FILIA
7C2511        MOVE CARG-CGC-FILIA    TO CMAS-CGC-FILIA-ST
001800        MOVE CARG-CGC-CONTR    TO CMAS-CGC-CONTR
001810        MOVE CARG-DT-CADASTR   TO CMAS-DT-CADASTR
001820        MOVE CARG-DT-CONVENIO  TO CMAS-DT-CONVENIO
001830        MOVE CARG-DT-ULT-ATUAL TO CMAS-DT-ULT-ATUAL
001840        MOVE CARG-NR-MAQUINAS  TO CMAS-NR-MAQUINAS
001850        MOVE CARG-CD-ATIVIDADE TO CMAS-CD-ATIVIDADE
001860        MOVE CARG-LIM-OPER     TO CMAS-LIM-OPER
001870        MOVE CARG-TP-CRED      TO CMAS-TP-CRED
001890        MOVE CARG-NOME-VEND    TO CMAS-NOME-VEND
001900        MOVE CARG-ENDER-VEND   TO CMAS-ENDER-VEND
001910        MOVE SPACES            TO CMAS-CIDAD-VEND
001920        MOVE CARG-CEP          TO CMAS-CEP
001930        MOVE SPACES            TO CMAS-UF
001940        MOVE CARG-RAZAO-COML   TO CMAS-RAZAO-COML
001950        MOVE CARG-FONE         TO CMAS-FONE
001960        MOVE CARG-TAXAS        TO CMAS-TAXAS
001970        MOVE SPACES            TO CMAS-RESERVA
001980
001990        IF   CARG-CEP < 1000 OR CARG-SFCEP NOT NUMERIC
002000             MOVE SPACES       TO CMAS-CEP-SEQ
002010             WRITE REG-CMAS
002020        ELSE
002030             MOVE CARG-SFCEP   TO CMAS-CEP-SEQ
002040             WRITE REG-CMAS.
002050
002060     GO TO LER-CARGATUA.
002070
002080 FIM-CARG.
002100     CLOSE  CARGATUA CMASTE32
002110
002120     STOP RUN.
