000010 IDENTIFICATION DIVISION.                                         RCORB001
000020 PROGRAM-ID. RCORB001.                                            RCORB001
000030*---------------------------------------------------------------*RCORB001
000040*                  B R A D E S C O   B B I                      *RCORB001
000050*---------------------------------------------------------------*RCORB001
000060* PROGRAMA......: RCORB001                                      *RCORB001
000070* DATA..........: 11/05/2026                                    *RCORB001
000080* PROGRAMADOR...: AGROLEG-1243                                  *RCORB001
000090* ANALISTA......: AGROLEG-1243                                  *RCORB001
000100*---------------------------------------------------------------*RCORB001
000110* OBJETIVO......: GERAR MENSAGEM COR0001E COM 4 GLEBAS PARA    *RCORB001
000120*                 TESTE DO MODULO RCOR8015 (COMPACTACAO WKT)   *RCORB001
000130*                 WKT: 230 PARES DE COORDS COM ESPACOS MULT.  *RCORB001
000140*                 MSG TOTAL: ~27.900 CHARS (DENTRO DO LIMIT)  *RCORB001
000150*---------------------------------------------------------------*RCORB001
000160* ENTRADAS......: NENHUMA                                       *RCORB001
000170* SAIDAS........: ARQSAIDA - MENSAGEM COR0001E (LRECL=30000)  *RCORB001
000180*                 ARQRELAT - RELATORIO DE CONFERENCIA          *RCORB001
000190*---------------------------------------------------------------*RCORB001
000200 ENVIRONMENT DIVISION.                                            RCORB001
000210 CONFIGURATION SECTION.                                           RCORB001
000220 SOURCE-COMPUTER. IBM-370.                                        RCORB001
000230 OBJECT-COMPUTER. IBM-370.                                        RCORB001
000240 INPUT-OUTPUT SECTION.                                            RCORB001
000250 FILE-CONTROL.                                                    RCORB001
000260     SELECT ARQ-SAIDA                                             RCORB001
000270         ASSIGN TO ARQSAIDA                                       RCORB001
000280         ORGANIZATION IS SEQUENTIAL                               RCORB001
000290         ACCESS IS SEQUENTIAL.                                    RCORB001
000300     SELECT ARQ-RELATO                                            RCORB001
000310         ASSIGN TO ARQRELAT                                       RCORB001
000320         ORGANIZATION IS SEQUENTIAL                               RCORB001
000330         ACCESS IS SEQUENTIAL.                                    RCORB001
000340 DATA DIVISION.                                                   RCORB001
000350 FILE SECTION.                                                    RCORB001
000360 FD  ARQ-SAIDA                                                    RCORB001
000370     RECORDING MODE IS F                                          RCORB001
000380     BLOCK CONTAINS 0 RECORDS                                     RCORB001
000390     RECORD CONTAINS 30000 CHARACTERS.                            RCORB001
000400 01  FS-SAIDA                  PIC X(30000).                      RCORB001
000410 FD  ARQ-RELATO                                                   RCORB001
000420     RECORDING MODE IS F                                          RCORB001
000430     BLOCK CONTAINS 0 RECORDS                                     RCORB001
000440     RECORD CONTAINS 0132 CHARACTERS.                             RCORB001
000450 01  FS-RELATO                 PIC X(0132).                       RCORB001
000460 WORKING-STORAGE SECTION.                                         RCORB001
000470*---------------------------------------------------------------*RCORB001
000480* CONTROLES GERAIS                                              *RCORB001
000490*---------------------------------------------------------------*RCORB001
000500 01  WRK-CONTROLE.                                                RCORB001
000510     05 WRK-MSG-POS            PIC 9(006) VALUE 1.                RCORB001
000520     05 WRK-TAM-MSG            PIC 9(006) VALUE ZEROS.            RCORB001
000530     05 WRK-WKT-POS            PIC 9(005) VALUE 1.                RCORB001
000535     05 WRK-WKT-REAL-LEN       PIC 9(005) VALUE ZEROS.            RCORB001
000540     05 WRK-CTR-COORD          PIC 9(003) VALUE ZEROS.            RCORB001
000550 01  WRK-MENSAGEM              PIC X(30000) VALUE SPACES.         RCORB001
000560 01  WRK-LINHA                 PIC X(0132)  VALUE SPACES.         RCORB001
000570*---------------------------------------------------------------*RCORB001
000580* COORDENADAS WKT COM ESPACOS MULTIPLOS (PARA TESTE COMPACTACAO)*RCORB001
000590**** PAR: 29 BYTES. 230 PARES = 6670 CHARS POR GLEBA          *RCORB001
000600**** HEADER: 13 CHARS + FOOTER: 27 CHARS = WKT: 6710 CHARS   *RCORB001
000610**** 4 GLEBAS: BLOCO 6+7+7A ~ 6.800 CHARS X 4 = ~27.200      *RCORB001
000620**** TOTAL MSG: ~27.900 CHARS (LIMITE RCOR16 = 30.000)        *RCORB001
000625**** APOS COMPACTACAO: ~21.000 CHARS (ECONOMIA ~25%)          *RCORB001
000630*---------------------------------------------------------------*RCORB001
000640 01  WRK-COORDENADAS.                                             RCORB001
000650     05 WRK-WKT-CABEC          PIC X(013) VALUE SPACES.           RCORB001
000660     05 WRK-COORD-G1           PIC X(029) VALUE SPACES.           RCORB001
000670     05 WRK-COORD-G1-F         PIC X(027) VALUE SPACES.           RCORB001
000680     05 WRK-COORD-G2           PIC X(029) VALUE SPACES.           RCORB001
000690     05 WRK-COORD-G2-F         PIC X(027) VALUE SPACES.           RCORB001
000700     05 WRK-COORD-G3           PIC X(029) VALUE SPACES.           RCORB001
000710     05 WRK-COORD-G3-F         PIC X(027) VALUE SPACES.           RCORB001
000720     05 WRK-COORD-G4           PIC X(029) VALUE SPACES.           RCORB001
000730     05 WRK-COORD-G4-F         PIC X(027) VALUE SPACES.           RCORB001
000740*---------------------------------------------------------------*RCORB001
000750* SNAPSHOT DAS GLEBAS PARA O RELATORIO                         *RCORB001
000760*---------------------------------------------------------------*RCORB001
000770 01  WRK-SNAP-GLEBAS.                                             RCORB001
000780     05 WRK-SNAP  OCCURS 4 TIMES.                                 RCORB001
000790        10 WRK-SNAP-IDENTC     PIC X(019) VALUE SPACES.           RCORB001
000800        10 WRK-SNAP-WKT        PIC X(080) VALUE SPACES.           RCORB001
000810        10 WRK-SNAP-POSINI     PIC 9(006) VALUE ZEROS.            RCORB001
000820        10 WRK-SNAP-WKTLEN     PIC 9(005) VALUE ZEROS.            RCORB001
000830*---------------------------------------------------------------*RCORB001
000840     COPY I#RUEC68.                                               RCORB001
000850*---------------------------------------------------------------*RCORB001
000860 PROCEDURE DIVISION.                                              RCORB001
000870*---------------------------------------------------------------*RCORB001
000880 0000-PRINCIPAL SECTION.                                          RCORB001
000890     PERFORM 1000-INICIALIZAR                                     RCORB001
000900     PERFORM 2000-MONTAR-BLOCOS-FIXOS                             RCORB001
000910     PERFORM 3000-CONSOLIDAR-MSG                                  RCORB001
000920     PERFORM 4000-GRAVAR-SAIDA                                    RCORB001
000930     PERFORM 5000-IMPRIMIR-RELATORIO                              RCORB001
000940     PERFORM 9000-FINALIZAR                                       RCORB001
000950     STOP RUN.                                                    RCORB001
000960 0000-99-FIM. EXIT.                                               RCORB001
000970*---------------------------------------------------------------*RCORB001
000980 1000-INICIALIZAR SECTION.                                        RCORB001
000990     OPEN OUTPUT ARQ-SAIDA                                        RCORB001
001000                  ARQ-RELATO                                      RCORB001
001010     INITIALIZE WRK-MENSAGEM                                      RCORB001
001020                WRK-SNAP-GLEBAS                                   RCORB001
001030     INITIALIZE RUEC68-COR0001-1                                  RCORB001
001040                RUEC68-COR0001-2                                  RCORB001
001050                RUEC68-COR0001-5                                  RCORB001
001060                RUEC68-COR0001-9                                  RCORB001
001070                RUEC68-COR0001-8                                  RCORB001
001080                RUEC68-COR0001-22                                 RCORB001
001090     MOVE 1 TO WRK-MSG-POS                                        RCORB001
001100*    Cabecalho WKT                                                RCORB001
001110     MOVE 'POLYGON  ((  '         TO WRK-WKT-CABEC                RCORB001
001120*    Coordenadas Gleba 1 (SP - Sao Paulo Norte)                  RCORB001
001130     MOVE '-46.62300    -23.54500   ,   '                         RCORB001
001140          TO WRK-COORD-G1                                         RCORB001
001150     MOVE '-46.62300    -23.54500   ))'                           RCORB001
001160          TO WRK-COORD-G1-F                                       RCORB001
001170*    Coordenadas Gleba 2 (SP - Sao Paulo Sul)                    RCORB001
001180     MOVE '-46.63000    -23.55000   ,   '                         RCORB001
001190          TO WRK-COORD-G2                                         RCORB001
001200     MOVE '-46.63000    -23.55000   ))'                           RCORB001
001210          TO WRK-COORD-G2-F                                       RCORB001
001220*    Coordenadas Gleba 3 (SP - Campinas)                         RCORB001
001230     MOVE '-47.20000    -22.90000   ,   '                         RCORB001
001240          TO WRK-COORD-G3                                         RCORB001
001250     MOVE '-47.20000    -22.90000   ))'                           RCORB001
001260          TO WRK-COORD-G3-F                                       RCORB001
001270*    Coordenadas Gleba 4 (SP - Ribeirao Preto)                   RCORB001
001280     MOVE '-47.50000    -23.10000   ,   '                         RCORB001
001290          TO WRK-COORD-G4                                         RCORB001
001300     MOVE '-47.50000    -23.10000   ))'                           RCORB001
001310          TO WRK-COORD-G4-F.                                      RCORB001
001320 1000-99-FIM. EXIT.                                               RCORB001
001330*---------------------------------------------------------------*RCORB001
001340**** BLOCOS FIXOS: CABECALHO, BENEFICIARIO, PROGRAMA, FINANC.  *RCORB001
001350*---------------------------------------------------------------*RCORB001
001360 2000-MONTAR-BLOCOS-FIXOS SECTION.                                RCORB001
001370     PERFORM 2010-MONTAR-BLOCO-1                                  RCORB001
001380     PERFORM 2020-MONTAR-BLOCO-2                                  RCORB001
001390     PERFORM 2030-MONTAR-BLOCO-5                                  RCORB001
001400     PERFORM 2040-MONTAR-BLOCO-9                                  RCORB001
001410     PERFORM 2050-MONTAR-BLOCO-8                                  RCORB001
001420     PERFORM 2060-MONTAR-BLOCO-22.                                RCORB001
001430 2000-99-FIM. EXIT.                                               RCORB001
001440*---------------------------------------------------------------*RCORB001
001450**** BLOCO 1: CABECALHO DA MENSAGEM COR0001E                   *RCORB001
001460*---------------------------------------------------------------*RCORB001
001470 2010-MONTAR-BLOCO-1 SECTION.                                     RCORB001
001480     MOVE 'COR0001E '             TO RUEC68-CODMSG                RCORB001
001490     MOVE '00000000000000000001'  TO RUEC68-NUMCTRLIF             RCORB001
001500     MOVE '12345678000195'        TO RUEC68-CNPJ-ENT-RESPONS      RCORB001
001510     MOVE '98765432000100'        TO RUEC68-CNPJ-PART             RCORB001
001520     MOVE '00000000001'           TO RUEC68-NUMREF-BCCOROR        RCORB001
001530     MOVE 20260101               TO RUEC68-DTEMS                  RCORB001
001540     MOVE 20270101               TO RUEC68-DTVENC                 RCORB001
001550     MOVE '001'                   TO RUEC68-CBASE-LEGAL-PRO       RCORB001
001560     MOVE '00000000000000001'     TO RUEC68-NUMCEDL-CRED-RURALIF  RCORB001
001570     MOVE '01'                    TO RUEC68-TP-INSTNTO-CRED       RCORB001
001580     MOVE '0000000000200000000'   TO RUEC68-VLR-TOTOP             RCORB001
001590     MOVE '0001'                  TO RUEC68-TP-CATG-EMIT.         RCORB001
001600 2010-99-FIM. EXIT.                                               RCORB001
001610*---------------------------------------------------------------*RCORB001
001620**** BLOCO 2: BENEFICIARIO {1000}                              *RCORB001
001630*---------------------------------------------------------------*RCORB001
001640 2020-MONTAR-BLOCO-2 SECTION.                                     RCORB001
001650     MOVE 'F'                     TO RUEC68-TP-PESSOA-EMIT        RCORB001
001660     MOVE '00000000000001'        TO RUEC68-CNPJ-CPF-EMIT         RCORB001
001670     MOVE '001'                   TO RUEC68-TPO-BENF.             RCORB001
001680 2020-99-FIM. EXIT.                                               RCORB001
001690*---------------------------------------------------------------*RCORB001
001700**** BLOCO 5: PROGRAMA / SUBPROGRAMA / FONTE / MUNICIPIO       *RCORB001
001710*---------------------------------------------------------------*RCORB001
001720 2030-MONTAR-BLOCO-5 SECTION.                                     RCORB001
001730     MOVE '0157'                  TO RUEC68-CODPROG-LINHA-CRED    RCORB001
001740     MOVE '0001'                  TO RUEC68-CODSUB-PROG           RCORB001
001750     MOVE '0001'                  TO RUEC68-TP-FNTE-REC           RCORB001
001760     MOVE 355030                 TO RUEC68-COD-MUNIC.             RCORB001
001770 2030-99-FIM. EXIT.                                               RCORB001
001780*---------------------------------------------------------------*RCORB001
001790**** BLOCO 9: DADOS FINANCEIROS / PRODUCAO                     *RCORB001
001800*---------------------------------------------------------------*RCORB001
001810 2040-MONTAR-BLOCO-9 SECTION.                                     RCORB001
001820     MOVE '00000000000001'        TO RUEC68-COD-EMPNMNT           RCORB001
001830     MOVE '1.1.1.1.01.01'         TO RUEC68-COD-SIST-PRODC        RCORB001
001840     MOVE '0000000000050000000'   TO RUEC68-VLR-PARCL-CRED        RCORB001
001850     MOVE '0000000000000000000'   TO RUEC68-VLR-PARCL-REC-PROP    RCORB001
001860     MOVE '0000000000000000000'   TO RUEC68-VLR-PARCL-SERVICOS    RCORB001
001870     MOVE '00000'                 TO RUEC68-ALQ-PROAGRO           RCORB001
001880     MOVE 750                    TO RUEC68-PERC-JUROS-ENCARFIN    RCORB001
001890     MOVE 1                      TO RUEC68-TP-ENCARG-FIN-COMPL    RCORB001
001900     MOVE '00000'                 TO RUEC68-PERC-RSC-STN          RCORB001
001910     MOVE '00000'                 TO RUEC68-PERC-RSC-FUND-CONSCL  RCORB001
001920     MOVE '00000'                 TO RUEC68-PERC-CST-EFT-TOTAL    RCORB001
001930     MOVE '00000200000'           TO RUEC68-AREA                  RCORB001
001940     MOVE 400000000000           TO RUEC68-QTD-ITEM-FINCD-N       RCORB001
001950     MOVE 400000000000           TO RUEC68-QTD-PRV-PRODC.         RCORB001
001960 2040-99-FIM. EXIT.                                               RCORB001
001970*---------------------------------------------------------------*RCORB001
001980**** BLOCO 8: PRODUTO CONSUMIDO {2300}                         *RCORB001
001990*---------------------------------------------------------------*RCORB001
002000 2050-MONTAR-BLOCO-8 SECTION.                                     RCORB001
002010     MOVE '0157'                  TO RUEC68-COD-PRODT-CONSCD.     RCORB001
002020 2050-99-FIM. EXIT.                                               RCORB001
002030*---------------------------------------------------------------*RCORB001
002040**** BLOCO 22: DATA DO MOVIMENTO                               *RCORB001
002050*---------------------------------------------------------------*RCORB001
002060 2060-MONTAR-BLOCO-22 SECTION.                                    RCORB001
002070     MOVE 20260501               TO RUEC68-DTMOVTO.               RCORB001
002080 2060-99-FIM. EXIT.                                               RCORB001
002090*---------------------------------------------------------------*RCORB001
002100**** CONSOLIDACAO: MONTA A MENSAGEM COMPLETA EM WRK-MENSAGEM   *RCORB001
002110**** CADA GLEBA: BLOCO 6 (52) + TAG+WKT+E (6724) + 7A (33)   *RCORB001
002120**** 4 GLEBAS = ~27.200 CHARS + FIXOS ~700 = TOTAL ~27.900    *RCORB001
002130**** APENAS O CONTEUDO REAL DO WKT E INCLUIDO (SEM PADDING)   *RCORB001
002140*---------------------------------------------------------------*RCORB001
002150 3000-CONSOLIDAR-MSG SECTION.                                     RCORB001
002160*    Concatena blocos fixos iniciais                             RCORB001
002170     STRING                                                       RCORB001
002180            RUEC68-COR0001-1     DELIMITED BY SIZE                RCORB001
002190            RUEC68-COR0001-2     DELIMITED BY SIZE                RCORB001
002200            RUEC68-COR0001-3     DELIMITED BY SIZE                RCORB001
002210            RUEC68-COR0001-5     DELIMITED BY SIZE                RCORB001
002220            RUEC68-COR0001-9     DELIMITED BY SIZE                RCORB001
002230            INTO WRK-MENSAGEM                                     RCORB001
002240            WITH POINTER WRK-MSG-POS                              RCORB001
002250*    Processa 4 glebas reutilizando RUEC68-COR0001-6/7/7A       RCORB001
002260     PERFORM 3010-APPEND-GLEBA-1                                  RCORB001
002270     PERFORM 3020-APPEND-GLEBA-2                                  RCORB001
002280     PERFORM 3030-APPEND-GLEBA-3                                  RCORB001
002290     PERFORM 3040-APPEND-GLEBA-4                                  RCORB001
002300*    Concatena blocos fixos finais                               RCORB001
002310     STRING                                                       RCORB001
002320            RUEC68-COR0001-8     DELIMITED BY SIZE                RCORB001
002330            RUEC68-COR0001-22    DELIMITED BY SIZE                RCORB001
002340            INTO WRK-MENSAGEM                                     RCORB001
002350            WITH POINTER WRK-MSG-POS                              RCORB001
002360     COMPUTE WRK-TAM-MSG = WRK-MSG-POS - 1.                       RCORB001
002370 3000-99-FIM. EXIT.                                               RCORB001
002380*---------------------------------------------------------------*RCORB001
002390**** GLEBA 1: BLOCO 6 + WKT REAL (SEM PADDING) + BLOCO 7A     *RCORB001
002400**** WKT-REAL-LEN = CONTEUDO EFETIVO GRAVADO PELA SECTION 3011 *RCORB001
002410*---------------------------------------------------------------*RCORB001
002420 3010-APPEND-GLEBA-1 SECTION.                                     RCORB001
002430     INITIALIZE RUEC68-COR0001-6                                  RCORB001
002440                RUEC68-COR0001-7                                  RCORB001
002450                RUEC68-COR0001-7A                                 RCORB001
002460     MOVE '0000000000000000001'   TO RUEC68-IDENTC-GLEBA          RCORB001
002470     MOVE '00000050000'           TO RUEC68-AREA-NCULTVD          RCORB001
002480     PERFORM 3011-WKT-GLEBA-1                                     RCORB001
002490     COMPUTE WRK-WKT-REAL-LEN    = WRK-WKT-POS - 1               RCORB001
002500     MOVE RUEC68-IDENTC-GLEBA    TO WRK-SNAP-IDENTC(1)            RCORB001
002510     MOVE RUEC68-WKT-PART(1:080) TO WRK-SNAP-WKT(1)              RCORB001
002520     MOVE WRK-MSG-POS            TO WRK-SNAP-POSINI(1)            RCORB001
002530     MOVE WRK-WKT-REAL-LEN       TO WRK-SNAP-WKTLEN(1)            RCORB001
002540     MOVE '00000050000'           TO RUEC68-AREA-NAO-CULT         RCORB001
002550     MOVE 355030                 TO RUEC68-COD-MUN-GLEB           RCORB001
002560     STRING                                                       RCORB001
002570            RUEC68-COR0001-6     DELIMITED BY SIZE                RCORB001
002580            '{2210}'             DELIMITED BY SIZE                RCORB001
002590            RUEC68-WKT-PART(1:WRK-WKT-REAL-LEN)                  RCORB001
002600                                 DELIMITED BY SIZE                RCORB001
002610            RUEC68-WKT-PART-E    DELIMITED BY SIZE                RCORB001
002620            RUEC68-COR0001-7A    DELIMITED BY SIZE                RCORB001
002630            INTO WRK-MENSAGEM                                     RCORB001
002640            WITH POINTER WRK-MSG-POS.                             RCORB001
002650 3010-99-FIM. EXIT.                                               RCORB001
002660*---------------------------------------------------------------*RCORB001
002670**** WKT GLEBA 1: 13 HEADER + 230x29 PARES + 27 FOOTER = 6710 *RCORB001
002680*---------------------------------------------------------------*RCORB001
002690 3011-WKT-GLEBA-1 SECTION.                                        RCORB001
002700     MOVE SPACES                  TO RUEC68-WKT-PART              RCORB001
002710     MOVE 1                      TO WRK-WKT-POS                   RCORB001
002720     STRING WRK-WKT-CABEC        DELIMITED BY SIZE                RCORB001
002730            INTO RUEC68-WKT-PART WITH POINTER WRK-WKT-POS        RCORB001
002740     PERFORM VARYING WRK-CTR-COORD FROM 1 BY 1                    RCORB001
002750         UNTIL WRK-CTR-COORD > 230                                RCORB001
002760         STRING WRK-COORD-G1     DELIMITED BY SIZE                RCORB001
002770                INTO RUEC68-WKT-PART                              RCORB001
002780                WITH POINTER WRK-WKT-POS                          RCORB001
002790         END-STRING                                               RCORB001
002800     END-PERFORM                                                  RCORB001
002810     STRING WRK-COORD-G1-F       DELIMITED BY SIZE                RCORB001
002820            INTO RUEC68-WKT-PART WITH POINTER WRK-WKT-POS.        RCORB001
002830 3011-99-FIM. EXIT.                                               RCORB001
002840*---------------------------------------------------------------*RCORB001
002850**** GLEBA 2: BLOCO 6 + WKT REAL (SEM PADDING) + BLOCO 7A     *RCORB001
002860*---------------------------------------------------------------*RCORB001
002870 3020-APPEND-GLEBA-2 SECTION.                                     RCORB001
002880     INITIALIZE RUEC68-COR0001-6                                  RCORB001
002890                RUEC68-COR0001-7                                  RCORB001
002900                RUEC68-COR0001-7A                                 RCORB001
002910     MOVE '0000000000000000002'   TO RUEC68-IDENTC-GLEBA          RCORB001
002920     MOVE '00000030000'           TO RUEC68-AREA-NCULTVD          RCORB001
002930     PERFORM 3021-WKT-GLEBA-2                                     RCORB001
002940     COMPUTE WRK-WKT-REAL-LEN    = WRK-WKT-POS - 1               RCORB001
002950     MOVE RUEC68-IDENTC-GLEBA    TO WRK-SNAP-IDENTC(2)            RCORB001
002960     MOVE RUEC68-WKT-PART(1:080) TO WRK-SNAP-WKT(2)              RCORB001
002970     MOVE WRK-MSG-POS            TO WRK-SNAP-POSINI(2)            RCORB001
002980     MOVE WRK-WKT-REAL-LEN       TO WRK-SNAP-WKTLEN(2)            RCORB001
002990     MOVE '00000030000'           TO RUEC68-AREA-NAO-CULT         RCORB001
003000     MOVE 355030                 TO RUEC68-COD-MUN-GLEB           RCORB001
003010     STRING                                                       RCORB001
003020            RUEC68-COR0001-6     DELIMITED BY SIZE                RCORB001
003030            '{2210}'             DELIMITED BY SIZE                RCORB001
003040            RUEC68-WKT-PART(1:WRK-WKT-REAL-LEN)                  RCORB001
003050                                 DELIMITED BY SIZE                RCORB001
003060            RUEC68-WKT-PART-E    DELIMITED BY SIZE                RCORB001
003070            RUEC68-COR0001-7A    DELIMITED BY SIZE                RCORB001
003080            INTO WRK-MENSAGEM                                     RCORB001
003090            WITH POINTER WRK-MSG-POS.                             RCORB001
003100 3020-99-FIM. EXIT.                                               RCORB001
003110*---------------------------------------------------------------*RCORB001
003120 3021-WKT-GLEBA-2 SECTION.                                        RCORB001
003130     MOVE SPACES                  TO RUEC68-WKT-PART              RCORB001
003140     MOVE 1                      TO WRK-WKT-POS                   RCORB001
003150     STRING WRK-WKT-CABEC        DELIMITED BY SIZE                RCORB001
003160            INTO RUEC68-WKT-PART WITH POINTER WRK-WKT-POS        RCORB001
003170     PERFORM VARYING WRK-CTR-COORD FROM 1 BY 1                    RCORB001
003180         UNTIL WRK-CTR-COORD > 230                                RCORB001
003190         STRING WRK-COORD-G2     DELIMITED BY SIZE                RCORB001
003200                INTO RUEC68-WKT-PART                              RCORB001
003210                WITH POINTER WRK-WKT-POS                          RCORB001
003220         END-STRING                                               RCORB001
003230     END-PERFORM                                                  RCORB001
003240     STRING WRK-COORD-G2-F       DELIMITED BY SIZE                RCORB001
003250            INTO RUEC68-WKT-PART WITH POINTER WRK-WKT-POS.        RCORB001
003260 3021-99-FIM. EXIT.                                               RCORB001
003270*---------------------------------------------------------------*RCORB001
003280**** GLEBA 3: BLOCO 6 + WKT REAL (SEM PADDING) + BLOCO 7A     *RCORB001
003290*---------------------------------------------------------------*RCORB001
003300 3030-APPEND-GLEBA-3 SECTION.                                     RCORB001
003310     INITIALIZE RUEC68-COR0001-6                                  RCORB001
003320                RUEC68-COR0001-7                                  RCORB001
003330                RUEC68-COR0001-7A                                 RCORB001
003340     MOVE '0000000000000000003'   TO RUEC68-IDENTC-GLEBA          RCORB001
003350     MOVE '00000040000'           TO RUEC68-AREA-NCULTVD          RCORB001
003360     PERFORM 3031-WKT-GLEBA-3                                     RCORB001
003370     COMPUTE WRK-WKT-REAL-LEN    = WRK-WKT-POS - 1               RCORB001
003380     MOVE RUEC68-IDENTC-GLEBA    TO WRK-SNAP-IDENTC(3)            RCORB001
003390     MOVE RUEC68-WKT-PART(1:080) TO WRK-SNAP-WKT(3)              RCORB001
003400     MOVE WRK-MSG-POS            TO WRK-SNAP-POSINI(3)            RCORB001
003410     MOVE WRK-WKT-REAL-LEN       TO WRK-SNAP-WKTLEN(3)            RCORB001
003420     MOVE '00000040000'           TO RUEC68-AREA-NAO-CULT         RCORB001
003430     MOVE 355040                 TO RUEC68-COD-MUN-GLEB           RCORB001
003440     STRING                                                       RCORB001
003450            RUEC68-COR0001-6     DELIMITED BY SIZE                RCORB001
003460            '{2210}'             DELIMITED BY SIZE                RCORB001
003470            RUEC68-WKT-PART(1:WRK-WKT-REAL-LEN)                  RCORB001
003480                                 DELIMITED BY SIZE                RCORB001
003490            RUEC68-WKT-PART-E    DELIMITED BY SIZE                RCORB001
003500            RUEC68-COR0001-7A    DELIMITED BY SIZE                RCORB001
003510            INTO WRK-MENSAGEM                                     RCORB001
003520            WITH POINTER WRK-MSG-POS.                             RCORB001
003530 3030-99-FIM. EXIT.                                               RCORB001
003540*---------------------------------------------------------------*RCORB001
003550 3031-WKT-GLEBA-3 SECTION.                                        RCORB001
003560     MOVE SPACES                  TO RUEC68-WKT-PART              RCORB001
003570     MOVE 1                      TO WRK-WKT-POS                   RCORB001
003580     STRING WRK-WKT-CABEC        DELIMITED BY SIZE                RCORB001
003590            INTO RUEC68-WKT-PART WITH POINTER WRK-WKT-POS        RCORB001
003600     PERFORM VARYING WRK-CTR-COORD FROM 1 BY 1                    RCORB001
003610         UNTIL WRK-CTR-COORD > 230                                RCORB001
003620         STRING WRK-COORD-G3     DELIMITED BY SIZE                RCORB001
003630                INTO RUEC68-WKT-PART                              RCORB001
003640                WITH POINTER WRK-WKT-POS                          RCORB001
003650         END-STRING                                               RCORB001
003660     END-PERFORM                                                  RCORB001
003670     STRING WRK-COORD-G3-F       DELIMITED BY SIZE                RCORB001
003680            INTO RUEC68-WKT-PART WITH POINTER WRK-WKT-POS.        RCORB001
003690 3031-99-FIM. EXIT.                                               RCORB001
003700*---------------------------------------------------------------*RCORB001
003710**** GLEBA 4: BLOCO 6 + WKT REAL (SEM PADDING) + BLOCO 7A     *RCORB001
003720*---------------------------------------------------------------*RCORB001
003730 3040-APPEND-GLEBA-4 SECTION.                                     RCORB001
003740     INITIALIZE RUEC68-COR0001-6                                  RCORB001
003750                RUEC68-COR0001-7                                  RCORB001
003760                RUEC68-COR0001-7A                                 RCORB001
003770     MOVE '0000000000000000004'   TO RUEC68-IDENTC-GLEBA          RCORB001
003780     MOVE '00000080000'           TO RUEC68-AREA-NCULTVD          RCORB001
003790     PERFORM 3041-WKT-GLEBA-4                                     RCORB001
003800     COMPUTE WRK-WKT-REAL-LEN    = WRK-WKT-POS - 1               RCORB001
003810     MOVE RUEC68-IDENTC-GLEBA    TO WRK-SNAP-IDENTC(4)            RCORB001
003820     MOVE RUEC68-WKT-PART(1:080) TO WRK-SNAP-WKT(4)              RCORB001
003830     MOVE WRK-MSG-POS            TO WRK-SNAP-POSINI(4)            RCORB001
003840     MOVE WRK-WKT-REAL-LEN       TO WRK-SNAP-WKTLEN(4)            RCORB001
003850     MOVE '00000080000'           TO RUEC68-AREA-NAO-CULT         RCORB001
003860     MOVE 355050                 TO RUEC68-COD-MUN-GLEB           RCORB001
003870     STRING                                                       RCORB001
003880            RUEC68-COR0001-6     DELIMITED BY SIZE                RCORB001
003890            '{2210}'             DELIMITED BY SIZE                RCORB001
003900            RUEC68-WKT-PART(1:WRK-WKT-REAL-LEN)                  RCORB001
003910                                 DELIMITED BY SIZE                RCORB001
003920            RUEC68-WKT-PART-E    DELIMITED BY SIZE                RCORB001
003930            RUEC68-COR0001-7A    DELIMITED BY SIZE                RCORB001
003940            INTO WRK-MENSAGEM                                     RCORB001
003950            WITH POINTER WRK-MSG-POS.                             RCORB001
003960 3040-99-FIM. EXIT.                                               RCORB001
003970*---------------------------------------------------------------*RCORB001
003980 3041-WKT-GLEBA-4 SECTION.                                        RCORB001
003990     MOVE SPACES                  TO RUEC68-WKT-PART              RCORB001
004000     MOVE 1                      TO WRK-WKT-POS                   RCORB001
004010     STRING WRK-WKT-CABEC        DELIMITED BY SIZE                RCORB001
004020            INTO RUEC68-WKT-PART WITH POINTER WRK-WKT-POS        RCORB001
004030     PERFORM VARYING WRK-CTR-COORD FROM 1 BY 1                    RCORB001
004040         UNTIL WRK-CTR-COORD > 230                                RCORB001
004050         STRING WRK-COORD-G4     DELIMITED BY SIZE                RCORB001
004060                INTO RUEC68-WKT-PART                              RCORB001
004070                WITH POINTER WRK-WKT-POS                          RCORB001
004080         END-STRING                                               RCORB001
004090     END-PERFORM                                                  RCORB001
004100     STRING WRK-COORD-G4-F       DELIMITED BY SIZE                RCORB001
004110            INTO RUEC68-WKT-PART WITH POINTER WRK-WKT-POS.        RCORB001
004120 3041-99-FIM. EXIT.                                               RCORB001
004130*---------------------------------------------------------------*RCORB001
004140**** GRAVACAO: ESCREVE WRK-MENSAGEM NO ARQUIVO DE SAIDA        *RCORB001
004150*---------------------------------------------------------------*RCORB001
004160 4000-GRAVAR-SAIDA SECTION.                                       RCORB001
004170     MOVE SPACES                  TO FS-SAIDA                     RCORB001
004180     MOVE WRK-MENSAGEM            TO FS-SAIDA                     RCORB001
004190     WRITE FS-SAIDA.                                              RCORB001
004200 4000-99-FIM. EXIT.                                               RCORB001
004210*---------------------------------------------------------------*RCORB001
004220**** RELATORIO: IMPRIME CONFERENCIA DOS DADOS MONTADOS         *RCORB001
004230*---------------------------------------------------------------*RCORB001
004240 5000-IMPRIMIR-RELATORIO SECTION.                                 RCORB001
004250     PERFORM 5010-RELAT-CABECALHO                                 RCORB001
004260     PERFORM 5020-RELAT-BLOCO1                                    RCORB001
004270     PERFORM 5030-RELAT-GLEBAS                                    RCORB001
004280     PERFORM 5040-RELAT-RODAPE.                                   RCORB001
004290 5000-99-FIM. EXIT.                                               RCORB001
004300*---------------------------------------------------------------*RCORB001
004310 5010-RELAT-CABECALHO SECTION.                                    RCORB001
004320     MOVE SPACES                  TO WRK-LINHA                    RCORB001
004330     MOVE '*** RCORB001 - MENSAGEM COR0001E COM 4 GLEBAS ***'     RCORB001
004340          TO WRK-LINHA                                            RCORB001
004350     WRITE FS-RELATO FROM WRK-LINHA                               RCORB001
004360     MOVE SPACES                  TO WRK-LINHA                    RCORB001
004370     WRITE FS-RELATO FROM WRK-LINHA.                              RCORB001
004380 5010-99-FIM. EXIT.                                               RCORB001
004390*---------------------------------------------------------------*RCORB001
004400 5020-RELAT-BLOCO1 SECTION.                                       RCORB001
004410     MOVE SPACES                  TO WRK-LINHA                    RCORB001
004420     MOVE '---- BLOCO 1 ----'     TO WRK-LINHA(1:017)             RCORB001
004430     WRITE FS-RELATO FROM WRK-LINHA                               RCORB001
004440     MOVE SPACES                  TO WRK-LINHA                    RCORB001
004450     MOVE 'CODMSG   : '           TO WRK-LINHA(1:011)             RCORB001
004460     MOVE RUEC68-CODMSG           TO WRK-LINHA(12:009)            RCORB001
004470     WRITE FS-RELATO FROM WRK-LINHA                               RCORB001
004480     MOVE SPACES                  TO WRK-LINHA                    RCORB001
004490     MOVE 'CNPJ RESP: '           TO WRK-LINHA(1:011)             RCORB001
004500     MOVE RUEC68-CNPJ-ENT-RESPONS TO WRK-LINHA(12:014)            RCORB001
004510     WRITE FS-RELATO FROM WRK-LINHA                               RCORB001
004520     MOVE SPACES                  TO WRK-LINHA                    RCORB001
004530     MOVE 'VLR TOTAL: '           TO WRK-LINHA(1:011)             RCORB001
004540     MOVE RUEC68-VLR-TOTOP        TO WRK-LINHA(12:019)            RCORB001
004550     WRITE FS-RELATO FROM WRK-LINHA                               RCORB001
004560     MOVE SPACES                  TO WRK-LINHA                    RCORB001
004570     MOVE 'TAM MSG  : '           TO WRK-LINHA(1:011)             RCORB001
004580     MOVE WRK-TAM-MSG             TO WRK-LINHA(12:006)            RCORB001
004590     WRITE FS-RELATO FROM WRK-LINHA                               RCORB001
004600     MOVE SPACES                  TO WRK-LINHA                    RCORB001
004610     WRITE FS-RELATO FROM WRK-LINHA.                              RCORB001
004620 5020-99-FIM. EXIT.                                               RCORB001
004630*---------------------------------------------------------------*RCORB001
004640 5030-RELAT-GLEBAS SECTION.                                       RCORB001
004650     MOVE SPACES                  TO WRK-LINHA                    RCORB001
004660     MOVE '---- GLEBAS {2200}/{2210} ----' TO WRK-LINHA(1:030)    RCORB001
004670     WRITE FS-RELATO FROM WRK-LINHA                               RCORB001
004680     MOVE SPACES                  TO WRK-LINHA                    RCORB001
004690     MOVE 'G1 IDENTC : '          TO WRK-LINHA(1:012)             RCORB001
004700     MOVE WRK-SNAP-IDENTC(1)      TO WRK-LINHA(13:019)            RCORB001
004710     MOVE ' WKTLEN: '             TO WRK-LINHA(33:009)            RCORB001
004720     MOVE WRK-SNAP-WKTLEN(1)      TO WRK-LINHA(42:005)            RCORB001
004730     MOVE ' POSINI: '             TO WRK-LINHA(48:009)            RCORB001
004740     MOVE WRK-SNAP-POSINI(1)      TO WRK-LINHA(57:006)            RCORB001
004750     WRITE FS-RELATO FROM WRK-LINHA                               RCORB001
004760     MOVE SPACES                  TO WRK-LINHA                    RCORB001
004770     MOVE 'G1 WKT(80): '          TO WRK-LINHA(1:012)             RCORB001
004780     MOVE WRK-SNAP-WKT(1)         TO WRK-LINHA(13:080)            RCORB001
004790     WRITE FS-RELATO FROM WRK-LINHA                               RCORB001
004800     MOVE SPACES                  TO WRK-LINHA                    RCORB001
004810     MOVE 'G2 IDENTC : '          TO WRK-LINHA(1:012)             RCORB001
004820     MOVE WRK-SNAP-IDENTC(2)      TO WRK-LINHA(13:019)            RCORB001
004830     MOVE ' WKTLEN: '             TO WRK-LINHA(33:009)            RCORB001
004840     MOVE WRK-SNAP-WKTLEN(2)      TO WRK-LINHA(42:005)            RCORB001
004850     MOVE ' POSINI: '             TO WRK-LINHA(48:009)            RCORB001
004860     MOVE WRK-SNAP-POSINI(2)      TO WRK-LINHA(57:006)            RCORB001
004870     WRITE FS-RELATO FROM WRK-LINHA                               RCORB001
004880     MOVE SPACES                  TO WRK-LINHA                    RCORB001
004890     MOVE 'G2 WKT(80): '          TO WRK-LINHA(1:012)             RCORB001
004900     MOVE WRK-SNAP-WKT(2)         TO WRK-LINHA(13:080)            RCORB001
004910     WRITE FS-RELATO FROM WRK-LINHA                               RCORB001
004920     MOVE SPACES                  TO WRK-LINHA                    RCORB001
004930     MOVE 'G3 IDENTC : '          TO WRK-LINHA(1:012)             RCORB001
004940     MOVE WRK-SNAP-IDENTC(3)      TO WRK-LINHA(13:019)            RCORB001
004950     MOVE ' WKTLEN: '             TO WRK-LINHA(33:009)            RCORB001
004960     MOVE WRK-SNAP-WKTLEN(3)      TO WRK-LINHA(42:005)            RCORB001
004970     MOVE ' POSINI: '             TO WRK-LINHA(48:009)            RCORB001
004980     MOVE WRK-SNAP-POSINI(3)      TO WRK-LINHA(57:006)            RCORB001
004990     WRITE FS-RELATO FROM WRK-LINHA                               RCORB001
005000     MOVE SPACES                  TO WRK-LINHA                    RCORB001
005010     MOVE 'G3 WKT(80): '          TO WRK-LINHA(1:012)             RCORB001
005020     MOVE WRK-SNAP-WKT(3)         TO WRK-LINHA(13:080)            RCORB001
005030     WRITE FS-RELATO FROM WRK-LINHA                               RCORB001
005040     MOVE SPACES                  TO WRK-LINHA                    RCORB001
005050     MOVE 'G4 IDENTC : '          TO WRK-LINHA(1:012)             RCORB001
005060     MOVE WRK-SNAP-IDENTC(4)      TO WRK-LINHA(13:019)            RCORB001
005070     MOVE ' WKTLEN: '             TO WRK-LINHA(33:009)            RCORB001
005080     MOVE WRK-SNAP-WKTLEN(4)      TO WRK-LINHA(42:005)            RCORB001
005090     MOVE ' POSINI: '             TO WRK-LINHA(48:009)            RCORB001
005100     MOVE WRK-SNAP-POSINI(4)      TO WRK-LINHA(57:006)            RCORB001
005110     WRITE FS-RELATO FROM WRK-LINHA                               RCORB001
005120     MOVE SPACES                  TO WRK-LINHA                    RCORB001
005130     MOVE 'G4 WKT(80): '          TO WRK-LINHA(1:012)             RCORB001
005140     MOVE WRK-SNAP-WKT(4)         TO WRK-LINHA(13:080)            RCORB001
005150     WRITE FS-RELATO FROM WRK-LINHA                               RCORB001
005160     MOVE SPACES                  TO WRK-LINHA                    RCORB001
005170     WRITE FS-RELATO FROM WRK-LINHA.                              RCORB001
005180 5030-99-FIM. EXIT.                                               RCORB001
005190*---------------------------------------------------------------*RCORB001
005200 5040-RELAT-RODAPE SECTION.                                       RCORB001
005210     MOVE SPACES                  TO WRK-LINHA                    RCORB001
005220     MOVE '*** FIM RCORB001 - TAM MSG: '   TO WRK-LINHA(1:028)    RCORB001
005230     MOVE WRK-TAM-MSG             TO WRK-LINHA(29:006)            RCORB001
005240     MOVE ' CHARS ***'             TO WRK-LINHA(35:010)           RCORB001
005250     WRITE FS-RELATO FROM WRK-LINHA.                              RCORB001
005260 5040-99-FIM. EXIT.                                               RCORB001
005270*---------------------------------------------------------------*RCORB001
005280 9000-FINALIZAR SECTION.                                          RCORB001
005290     CLOSE ARQ-SAIDA                                              RCORB001
005300           ARQ-RELATO.                                            RCORB001
005310 9000-99-FIM. EXIT.                                               RCORB001
