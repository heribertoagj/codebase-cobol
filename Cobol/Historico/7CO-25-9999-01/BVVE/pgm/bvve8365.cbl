000010 ID DIVISION.
000020 PROGRAM-ID.     BVVE8365.
000030 AUTHOR.         IVONEIDE-POLEN.
000040 DATE-WRITTEN.   24/08/94.
000050 DATE-COMPILED.
000060******************************************************
000070*    PROGRAMADOR  :  GIOVANE DE ANDRADE CPM SISTEMAS *
000080*    SUPERVISOR   :  LUIS               CPM SISTEMAS *
000090*    ANALISTA     :  ROSANA   GP 90                  *
000100*    OBS.  ESTE PROGRAMA EH UMA COPIA DO ELOE8365.   *
000110******************************************************
000120*                                                    *
000130*  LE ARQUIVO DE REGISTROS PENDENTES DA CARTEIRA     *
000140* 'D', SUMARIZAR VALORES POR AG/CONTA/DATA E         *
000150*  BATER CONTRA O ARQUIVO DE MESTRE DO PROPONEN-     *
000160*  TE DA CARTEIRA 'D' PARA OBTER O NOME DA           *
000170*  AGENCIA E DO PROPONENTE.                          *
000180*                                                    *
000190******************************************************
000200****************************************************************
000210*    U  L  T  I  M  A    A  L  T  E  R  A  C  A  O   :         *
000220****************************************************************
000230*                                                              *
000240* PROGRAMADOR...: ROMULO MARCHINA SOARES - CPM                 *
000250* ANALISTA......: VALERIA                - CPM                 *
000260* DATA..........: 21/10/1998                                   *
000270*                                                              *
000280* OBJETIVO......: CONVERSAO DE TODAS AS DATAS CONTEMPLANDO A   *
000290*                 VIRADA DO SECULO.                            *
000300*                                                              *
000310****************************************************************
000320*
000330 ENVIRONMENT DIVISION.
000340 CONFIGURATION SECTION.
000350*
000360 SPECIAL-NAMES.
000370     DECIMAL-POINT IS COMMA.
000380*
000390 INPUT-OUTPUT SECTION.
000400 FILE-CONTROL.
000410*
000420     SELECT  ARQPEND         ASSIGN  UT-S-ARQPEND.
000430     SELECT  CADPROP         ASSIGN  UT-S-CADPROP.
000440     SELECT  CARTEID         ASSIGN  UT-S-CARTEID.
000450*
000460 DATA DIVISION.
000470 FILE SECTION.
000480*
000490 FD  CADPROP
000500     BLOCK 0
000510     RECORDING F
000520     LABEL RECORD IS OMITTED.
000530*
000540     SKIP2
000550*------------------------------------------------------------*
000560* RECSZ=110      F2-MESTRE DE PROPONENTES    -INC I#DECEAQ   *
000570* INC I#DECEAQ (NOVA)   - TAMANHO = 110                      *
000580*------------------------------------------------------------*
000590     SKIP1
000600 01  MWORKF2.
000610     02  CADPROP-RAMO                    PIC X.
000620     02  MCHAVEF2.
000630         03  CADPROP-AGENCIA             PIC 9(05).
000640         03  MCONTCOR.
000650             04  CADPROP-CONTA           PIC 9(07).
000660             04  CADPROP-DIGCTA          PIC X.
000670         03  CADPROP-CARTEIRA            PIC X.
000680     02  CADPROP-NOMAGEN                 PIC X(10).
000690     02  MREGZERO.
000700         03  MDTMOVTO                    PIC 9(08).
000710         03  FILLER                      PIC X(77).
000720     02  MDETPROP                REDEFINES       MREGZERO.
000730         03  CADPROP-NOMPROP             PIC X(40).
000740         03  MSALDO                      PIC 9(13)V99    COMP-3.
000750         03  MRAZAO                      PIC 9(05)       COMP-3.
000760         03  MDTULTMV                    PIC 9(09)       COMP-3.
000770         03  MPESSOA                     PIC X.
000780         03  MCODATIV                    PIC X(02).
000790         03  MCGCCPF.
FS2511*            04  MNUMEROC                PIC 9(11)       COMP-3.
FS2511*            04  MFILIALC                PIC 9(05)       COMP-3.
FS2511             04  MNUMEROC                PIC X(11).
FS2511             04  MFILIALC                PIC X(04).
000820             04  MCONTROC                PIC 9(02).
000830         03  FILLER                      PIC X(15).
000840     SKIP3
000850*
000860 FD  ARQPEND
000870     BLOCK 0
000880     RECORDING F
000890     LABEL RECORD IS OMITTED.
000900*
000910     SKIP2
000920*------------------------------------------------------------*
000930* INC I#DECEAR (NOVA)    ==> TAMANHO ==> 220                 *
000940*------------------------------------------------------------*
000950     SKIP1
000960 01  REG-ARQPEND.
000970     02  DCHAVE2D.
000980         03  ARQPEND-AGENCIA            PIC 9(05).
000990         03  DCONTCOR.
001000             04  ARQPEND-CONTA          PIC 9(07).
001010             04  ARQPEND-DIGITO         PIC X.
001020         03  ARQPEND-CARTEIRA           PIC X.
001030         03  DNOSNUME.
001040             04  DNNCODAT               PIC 9(03).
001050             04  DNNUMERO               PIC 9(06).
001060         03  DNOSNUME-R  REDEFINES  DNOSNUME   PIC 9(09).
001070     02  ARQPEND-DTDEB                  PIC 9(08).
001080     02  ARQPEND-VALOR                  PIC 9(11)V99    COMP-3.
001090     02  DRAMO                          PIC X.
001100     02  DDEPOSIT                       PIC 9(07)       COMP-3.
001110     02  DAUTORIZ                       PIC 9(02).
001120     02  DSEUNUME                       PIC X(15).
001130     02  DDUPNP                         PIC X.
001140     02  DCEV                           PIC 9.
001150     02  DCDA                           PIC X.
001160     02  DLP                            PIC 9.
001170     02  DCOMISS                        PIC 9.
001180     02  DJUCOTAX                       PIC 9(11)V99    COMP-3.
001190     02  DPRACAS                        PIC X.
001200     02  DNOMESAC                       PIC X(40).
001210     02  REGPENDE.
001220         03  DENDESAC                   PIC X(44).
001230         03  ARQPEND-DTDESC             PIC 9(09)       COMP-3.
001240         03  DMOTIVO                    PIC 9.
001250         03  DCTP                       PIC X.
001260         03  DCCB                       PIC X.
001270     02  REGTBAIX                REDEFINES       REGPENDE.
001280         03  FILLER                     PIC X(04).
001290         03  DDTBAIXA                   PIC 9(09)       COMP-3.
001300         03  DDTPGTO                    PIC 9(09)       COMP-3.
001310         03  DAGCOBRA                   PIC 9(05)       COMP-3.
001320         03  DVALORCB                   PIC 9(11)V99    COMP-3.
001330         03  DJUMORAC                   PIC 9(07)V99    COMP-3.
001340         03  DISMORAC                   PIC 9(07)V99    COMP-3.
001350         03  DDTTRANS                   PIC 9(09)       COMP-3.
001360         03  DDTCONTB                   PIC 9(09)       COMP-3.
001370         03  FILLER                     PIC X(08).
001380     02  DCGCSAC                        PIC 9(15)       COMP-3.
001390     02  DCEPSAC                        PIC 9(05)       COMP-3.
001400     02  DTAXAS.
001410         03  DTAXA                      PIC 9(03)V99    COMP-3.
001420         03  DTXCOR                     PIC 9(03)V99    COMP-3.
001430         03  DEN                        PIC X.
001440     02  DMORDIA                        PIC 9(11)V99    COMP-3.
001450     02  DISOF                          PIC 9(09)V99    COMP-3.
001460     02  DNOVOCEP.
001470         03  DCEPNOVO                   PIC 9(05)       COMP-3.
001480         03  DSUFIXO                    PIC X(03).
001490     02  DVALTIT                        PIC 9(11)V99    COMP-3.
001500     02  DVALTIT-R               REDEFINES       DVALTIT.
001510         03  DBANCOCOBR                 PIC 9(03)       COMP-3.
001520         03  DVALBONIF                  PIC 9(07)V99    COMP-3.
001530     02  DIDENURV                       PIC X(01).
001540     02  DDTEMIS                        PIC 9(09)       COMP-3.
001550     02  FILLER                         PIC X(05).
001560     SKIP3
001570*
001580 FD  CARTEID
001590     BLOCK 0
001600     RECORDING F
001610     LABEL RECORD IS OMITTED.
001620*
001630 01  REG-CARTEID.
001640     05  CARTEID-AGEN            PIC  9(05)     COMP-3.
001650     05  CARTEID-CONTA           PIC  9(07)     COMP-3.
001660     05  CARTEID-DTDEBI.
001670         10  CARTEID-AADEBI      PIC  9(04).
001680         10  CARTEID-MMDEBI      PIC  9(02).
001690         10  CARTEID-DDDEBI      PIC  9(02).
001700     05  CARTEID-VALOR           PIC S9(13)V99  COMP-3.
001710     05  CARTEID-NOMPROP         PIC  X(35).
001720     05  CARTEID-NOMAGEN         PIC  X(10).
001730     05  FILLER                  PIC  X(02).
001740*
001750 WORKING-STORAGE SECTION.
001760*
001770*** AREA DA POOL7600 ***
-INC I#BVGEDH
001790*
001800 01  FILLER.
001810*
001820     05  CH-FIM-ARQPEND          PIC  X(01)  VALUE  SPACES.
001830         88  FIM-ARQPEND         VALUE '1'.
001840*
001850     05  WK-VALOR-SUM            PIC  9(15)V99.
001860*
001870     05  WK-NOMPROP              PIC  X(35).
001880     05  WK-NOMAGEN              PIC  X(10).
001890*
001900     05  WK-QB-ANT.
001910         10  WK-AGENCIA-ANT      PIC  9(05).
001920         10  WK-CONTA-ANT        PIC  9(07).
001930         10  WK-DATA-ANT.
001940             15  WK-DD-ANT       PIC  9(02).
001950             15  WK-MM-ANT       PIC  9(02).
001960             15  WK-AA-ANT       PIC  9(04).
001970*
001980     05  WK-QB-ATU.
001990         10  WK-AGENCIA-ATU      PIC  9(05).
002000         10  WK-CONTA-ATU        PIC  9(07).
002010         10  WK-DATA-ATU         PIC  9(08).
002020*
002030     05  WK-CHAVE-ARQPEND.
002040         10  WK-AGENCIA-ARQPEND  PIC  9(05).
002050         10  WK-CONTA-ARQPEND    PIC  9(07).
002060*
002070     05  WK-CHAVE-CADPROP.
002080         10  WK-AGENCIA-CADPROP  PIC  9(05).
002090         10  WK-CONTA-CADPROP    PIC  9(07).
002100*
002110     05  AC-LIDOS-CADPROP        PIC  9(15)  COMP-3  VALUE  ZEROS.
002120     05  AC-DESPR-CADPROP        PIC  9(15)  COMP-3  VALUE  ZEROS.
002130     05  AC-LIDOS-ARQPEND        PIC  9(15)  COMP-3  VALUE  ZEROS.
002140     05  AC-DESPR-ARQPEND-D      PIC  9(15)  COMP-3  VALUE  ZEROS.
002150     05  AC-DESPR-ARQPEND-DATA   PIC  9(15)  COMP-3  VALUE  ZEROS.
002160     05  AC-ARQPEND-CCOR         PIC  9(15)  COMP-3  VALUE  ZEROS.
002170     05  AC-ARQPEND-SCOR         PIC  9(15)  COMP-3  VALUE  ZEROS.
002180     05  AC-GRAV-CARTEID         PIC  9(15)  COMP-3  VALUE  ZEROS.
002190*
002200     05  WK-DATSIS               PIC  9(08).
002210     05  FILLER REDEFINES WK-DATSIS.
002220         10  WK-ANOSIS           PIC  9(004).
002230         10  WK-MESSIS           PIC  9(002).
002240         10  WK-DIASIS           PIC  9(002).
002250*
002260     05  WK-DTAUX1.
002270         10  WK-DDAUX1           PIC  9(002).
002280         10  WK-MMAUX1           PIC  9(002).
002290         10  WK-AAAUX1           PIC  9(004).
002300     05  WK-DTAUX1-R  REDEFINES  WK-DTAUX1
002310                                 PIC  9(008).
002320*
002330     05  WK-DTSEL.
002340         10  WK-AASEL            PIC  9(004).
002350         10  WK-MMSEL            PIC  9(002).
002360         10  WK-DDSEL            PIC  9(002).
002370*
002380     05  WK-DTARQ.
002390         10  WK-AAARQ            PIC  9(004).
002400         10  WK-MMARQ            PIC  9(002).
002410         10  WK-DDARQ            PIC  9(002).
002420*
002430     05  WK-NOME-POOL1205        PIC  X(008)  VALUE  'POOL1205'.
002440     05  WK-POOL1205.
002450         10  WK-DATA-POOL1205    PIC  9(008) COMP-3.
002460         10  WK-STATUS-POOL1205  PIC  X(001).
002470         10  FILLER              PIC  X(060).
002480     05  WRK-MENS                PIC  X(050) VALUE SPACES.
002490*
002500     05  WK-NOME-POOL0025        PIC  X(008)  VALUE  'POOL0025'.
002510     05  WK-DTENT-POOL0025       PIC  9(009)  COMP-3.
002520     05  WK-NDIAS-POOL0025       PIC S9(005)  COMP-3.
002530     05  WK-DTSAI-POOL0025       PIC  9(009)  COMP-3.
002540*
002550     05  WK-DIAS-UTEIS           PIC  9(005).
002560*
002570     05  WRK-DATA-OLD            PIC  9(008) VALUE ZEROS.
002580     05  FILLER REDEFINES  WRK-DATA-OLD.
002590         10  WRK-DIA-OLD         PIC  9(002).
002600         10  WRK-MES-OLD         PIC  9(002).
002610         10  WRK-ANO-OLD         PIC  9(004).
002620*
002630     05  WRK-DATA-NEW            PIC  9(008) VALUE ZEROS.
002640     05  FILLER REDEFINES  WRK-DATA-NEW.
002650         10  WRK-ANO-NEW         PIC  9(004).
002660         10  WRK-MES-NEW         PIC  9(002).
002670         10  WRK-DIA-NEW         PIC  9(002).
002680*
002690     EJECT
002700*
002710 PROCEDURE DIVISION.
002720*
002730     OPEN  INPUT   ARQPEND
002740                   CADPROP
002750           OUTPUT  CARTEID.
002760*
002770     CALL 'POOL7600' USING DATA-HORA.
002780     MOVE DT-AAAAMMDD   TO  WK-DATSIS.
002790     MOVE    WK-ANOSIS    TO    WK-AAAUX1.
002800     MOVE    WK-MESSIS    TO    WK-MMAUX1.
002810     MOVE    WK-DIASIS    TO    WK-DDAUX1.
002820     MOVE    WK-DTAUX1-R  TO    WK-DTENT-POOL0025.
002830     MOVE    ZEROS        TO    WK-NDIAS-POOL0025.
002840     MOVE    ZEROS        TO    WK-DIAS-UTEIS.
002850*
002860     PERFORM  0900-CALCULA-DATA  UNTIL  WK-DIAS-UTEIS
002870                                 GREATER 6.
002880*
002890     MOVE  WK-DTSAI-POOL0025  TO  WK-DTAUX1-R.
002900     MOVE  WK-DDAUX1          TO  WK-DDSEL.
002910     MOVE  WK-MMAUX1          TO  WK-MMSEL.
002920     MOVE  WK-AAAUX1          TO  WK-AASEL.
002930*
002940     PERFORM  R1000-LEITURA-ARQPEND  THRU  R1000-EXIT.
002950*
002960     PERFORM  R2000-LEITURA-CADPROP   THRU  R2000-EXIT.
002970*
002980     PERFORM  R3000-PROCESSA  THRU  R3000-EXIT UNTIL  FIM-ARQPEND.
002990*
003000     CLOSE  ARQPEND
003010            CADPROP
003020            CARTEID.
003030*
003040     DISPLAY '********************************'
003050     DISPLAY '*             BVVE8365         *'
003060     DISPLAY '********************************'
003070     DISPLAY '*                              *'
003080     DISPLAY 'TOTAL LIDOS NO ARQPEND.........= ' AC-LIDOS-ARQPEND
003090     DISPLAY '*                              * '
003100     DISPLAY 'TOTAL DESPR NO ARQPEND (D).....= '
003110                                       AC-DESPR-ARQPEND-D
003120     DISPLAY '*                              * '
003130     DISPLAY 'TOTAL DESPR NO ARQPEND (DATA)..= '
003140                                       AC-DESPR-ARQPEND-DATA
003150     DISPLAY '*                              * '
003160     DISPLAY 'ARQPEND  COM CORRESPONDENTE....= ' AC-ARQPEND-CCOR
003170     DISPLAY '*                              * '
003180     DISPLAY 'ARQPEND  SEM CORRESPONDENTE....= ' AC-ARQPEND-SCOR
003190     DISPLAY '*                              * '
003200     DISPLAY 'TOTAL LIDOS NO CADPROP.........= ' AC-LIDOS-CADPROP
003210     DISPLAY '*                              * '
003220     DISPLAY 'TOTAL DESPR NO CADPROP.........= ' AC-DESPR-CADPROP
003230     DISPLAY '*                              * '
003240     DISPLAY 'TOTAL GRAVADOS NO CARTEID......= ' AC-GRAV-CARTEID
003250     DISPLAY '*                              * '
003260     DISPLAY '********************************'
003270*
003280     STOP RUN.
003290*
003300     EJECT
003310*
003320******************************************************************
003330*              CALCULA DATA PARAMETRO DE SELECAO                 *
003340******************************************************************
003350 0900-CALCULA-DATA  SECTION.
003360*
003370     ADD  -1          TO        WK-NDIAS-POOL0025.
003380     CALL 'POOL0080'  USING     WK-NOME-POOL0025
003390                                WK-DTENT-POOL0025
003400                                WK-NDIAS-POOL0025
003410                                WK-DTSAI-POOL0025.
003420*
003430     MOVE  WK-DTSAI-POOL0025  TO  WRK-DATA-OLD.
003440     MOVE  WRK-DIA-OLD        TO  WRK-DIA-NEW.
003450     MOVE  WRK-MES-OLD        TO  WRK-MES-NEW.
003460     MOVE  WRK-ANO-OLD        TO  WRK-ANO-NEW.
003470     MOVE  WRK-DATA-NEW       TO  WK-DATA-POOL1205.
003480     MOVE  'N'                TO  WK-STATUS-POOL1205.
003490     CALL 'POOL0080'  USING       WK-NOME-POOL1205
003500                                  WK-POOL1205
003510                                  WRK-MENS.
003520*
003530     IF  WK-STATUS-POOL1205  NOT  EQUAL  'U'
003540         NEXT  SENTENCE
003550     ELSE
003560         ADD +1  TO  WK-DIAS-UTEIS.
003570*
003580 0900-EXIT.
003590     EXIT.
003600*
003610******************************************************************
003620*                  LEITURA DO ARQUIVO ARQPEND                    *
003630******************************************************************
003640 R1000-LEITURA-ARQPEND  SECTION.
003650*
003660 R1000-LE.
003670*
003680     READ  ARQPEND  AT  END
003690           MOVE  1            TO  CH-FIM-ARQPEND
003700           MOVE  HIGH-VALUES  TO  WK-CHAVE-ARQPEND
003710           MOVE  HIGH-VALUES  TO  WK-QB-ATU
003720           GO TO R1000-EXIT.
003730*
003740     ADD   +1               TO  AC-LIDOS-ARQPEND.
003750*
003760     IF  ARQPEND-CARTEIRA  NOT  EQUAL 'D'
003770         ADD   +1           TO  AC-DESPR-ARQPEND-D
003780         GO TO R1000-LE.
003790*
003800     MOVE  ARQPEND-DTDESC   TO  WK-DTAUX1-R.
003810     MOVE  WK-MMAUX1        TO  WK-MMARQ.
003820     MOVE  WK-DDAUX1        TO  WK-DDARQ.
003830     MOVE  WK-AAAUX1        TO  WK-AAARQ.
003840     IF  WK-DTARQ  GREATER  WK-DTSEL
003850         ADD   +1           TO  AC-DESPR-ARQPEND-DATA
003860         GO TO R1000-LE.
003870*
003880     MOVE  ARQPEND-AGENCIA  TO  WK-AGENCIA-ATU.
003890     MOVE  ARQPEND-CONTA    TO  WK-CONTA-ATU.
003900     MOVE  ARQPEND-DTDEB    TO  WK-DATA-ATU.
003910*
           MOVE  ARQPEND-AGENCIA  TO  WK-AGENCIA-ARQPEND.
           MOVE  ARQPEND-CONTA    TO  WK-CONTA-ARQPEND.
003940*
003950 R1000-EXIT.
003960     EXIT.
003970*
003980******************************************************************
003990*                  LEITURA DO CADASTRO DO PROPONENTE             *
004000******************************************************************
004010 R2000-LEITURA-CADPROP  SECTION.
004020*
004030 R2000-LE.
004040*
004050     READ  CADPROP  AT  END
004060           MOVE  HIGH-VALUES  TO  WK-CHAVE-CADPROP
004070           GO TO R2000-EXIT.
004080*
004090     ADD  +1  TO  AC-LIDOS-CADPROP.
004100*
004110     IF  CADPROP-CARTEIRA  NOT  EQUAL  'D'
004120         ADD  +1  TO  AC-DESPR-CADPROP
004130         GO TO R2000-LE.
004140*
004150     MOVE  CADPROP-AGENCIA  TO  WK-AGENCIA-CADPROP
004160     MOVE  CADPROP-CONTA    TO  WK-CONTA-CADPROP.
004170*
004180 R2000-EXIT.
004190     EXIT.
004200*
004210******************************************************************
004220*                         PROCESSAMENTO                          *
004230******************************************************************
004240 R3000-PROCESSA  SECTION.
004250*
004260 R3000-BATIMENTO.
004270*
004280     IF  WK-CHAVE-ARQPEND  EQUAL  WK-CHAVE-CADPROP
004290         MOVE   CADPROP-NOMPROP  TO  WK-NOMPROP
004300         MOVE   CADPROP-NOMAGEN  TO  WK-NOMAGEN
004310         ADD    +1               TO  AC-ARQPEND-CCOR
004320     ELSE
004330         IF  WK-CHAVE-ARQPEND  LESS   WK-CHAVE-CADPROP
004340             MOVE   SPACES       TO  WK-NOMPROP
004350             MOVE   SPACES       TO  WK-NOMAGEN
004360             ADD    +1           TO  AC-ARQPEND-SCOR
004370         ELSE
004380             PERFORM  R2000-LEITURA-CADPROP  THRU  R2000-EXIT
004390             GO TO R3000-BATIMENTO.
004400*
004410     MOVE  ZEROS   TO  WK-VALOR-SUM.
004420     MOVE  WK-QB-ATU  TO  WK-QB-ANT.
004430*
004440     PERFORM  R3100-SUMARIZA  UNTIL  WK-QB-ATU  NOT  EQUAL
004450                                     WK-QB-ANT.
004460*
004470     MOVE  SPACES          TO  REG-CARTEID.
004480     MOVE  WK-AGENCIA-ANT  TO  CARTEID-AGEN
004490     MOVE  WK-CONTA-ANT    TO  CARTEID-CONTA
004500     MOVE  WK-AA-ANT       TO  CARTEID-AADEBI
004510     MOVE  WK-MM-ANT       TO  CARTEID-MMDEBI
004520     MOVE  WK-DD-ANT       TO  CARTEID-DDDEBI
004530     MOVE  WK-VALOR-SUM    TO  CARTEID-VALOR
004540     MOVE  WK-NOMPROP      TO  CARTEID-NOMPROP
004550     MOVE  WK-NOMAGEN      TO  CARTEID-NOMAGEN.
004560*
004570     WRITE  REG-CARTEID.
004580     ADD    +1   TO  AC-GRAV-CARTEID.
004590*
004600 R3000-EXIT.
004610     EXIT.
004620*
004630******************************************************************
004640*               SUMARIZA VALOR                                   *
004650******************************************************************
004660 R3100-SUMARIZA  SECTION.
004670*
004680     ADD  ARQPEND-VALOR  TO  WK-VALOR-SUM.
004690     PERFORM  R1000-LEITURA-ARQPEND  THRU  R1000-EXIT.
004700*
004710 R3100-EXIT.
004720     EXIT.
