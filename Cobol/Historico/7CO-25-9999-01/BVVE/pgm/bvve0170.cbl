000010**************************
000020 IDENTIFICATION DIVISION.
000030**************************
000040
000050 PROGRAM-ID.  BVVE0170.
000060*REMARKS.
000070*
000080*    ********************** BVVE0170 ***********************
000090*    *                               ----------            *
000100*    *     PROGRAMA                 � BVVE0170 |           *
000110*    *                               ----------            *
000120*    *     PROGRAMADORA   - MARTA HAYASHI     - CPM        *
000130*    *     SUPERVISOR     - LUIZ              - CPM        *
000140*    *     ANALISTA       - LUCIO             - GRP. 34    *
000150*    *     DATA           - SETEMBRO / 1995                *
000160*    *     MANUTENCAO     - LUCIO - COLOCAR MSG NA VOLTA   *
000170*    *                      DO HELP, COLOCAR NUM.ESTAB.    *
000180*    *                      APOS FLAG NA AREA DE COMUNIC.  *
000190*    *                    - ACERTAR QDO FOR NOME ESPECIAL  *
000210*    *-----------------------------------------------------*
000220*    *                    OBJETIVO                         *
000230*    *                                                     *
000240*    *     DISPONIBILIZAR OPCOES PARA PESQUISA DE DADOS    *
000250*    *     CADASTRAIS DO ESTABELECIMENTO.                  *
000260*    *                                                     *
000270************************************************************
000280****************************************************************
000290*                A  L  T  E  R  A  C  A  O                     *
000300****************************************************************
000310*                                                              *
000320* PROGRAMADOR...: ROMULO MARCHINA SOARES - CPM                 *
000330* ANALISTA......: VALERIA                - CPM                 *
000340* DATA..........: 01/10/1998                                   *
000350*                                                              *
000360* OBJETIVO......: CONVERSAO DE TODAS AS DATAS CONTEMPLANDO A   *
000370*                 VIRADA DO SECULO NA SIGLA BVVE COM OS RES-   *
000380*                 PECTIVOS NUMEROS DO BACC.                    *
000390****************************************************************
000400*              U L T I M A   A L T E R A C A O                 *
000420*                                                              *
000430*     PROGRAMADOR : ALTAIR KLOTZ           - CPM/FPOLIS        *
000440*     ANALISTA    : CRISTINA               - GRUPO 90          *
000450*     DATA        : 06/08/1999                                 *
000460*                                                              *
000470*     OBJETIVO    :                                            *
000480*       FAZER COM QUE O PGM SOMENTE PESQUISE OS DADOS REFE-    *
000490*       RENTES A AGENCIA DO USUARIO                            *
000500*                                                              *
000510*==============================================================*
000520
000530     EJECT
000540***********************
000550 ENVIRONMENT DIVISION.
000560***********************
000570
000580 CONFIGURATION SECTION.
000590*---------------------*
000600
000610 SPECIAL-NAMES.
000630
000640    EJECT
000650****************
000660 DATA DIVISION.
000670****************
000680
000690 WORKING-STORAGE SECTION.
000700*-----------------------*
000710
000720*-------------------------------------------------------*
000730*        A R E A    D A   P O O L 7 6 0 0
000740*-------------------------------------------------------*
-INC I#BVGEDH
000760
000770 01  WRK-DATA-POOL7600        PIC 9(08).
000780 01  FILLER REDEFINES WRK-DATA-POOL7600.
000790     03  WRK-AAAA-POOL7600    PIC 9(04).
000800     03  WRK-MM-POOL7600      PIC 9(02).
000810     03  WRK-DD-POOL7600      PIC 9(02).
000820
000840     03  WRK-DD-POOL7600-E    PIC 9(02).
000850     03  FILLER               PIC X(01) VALUE '/'.
000860     03  WRK-MM-POOL7600-E    PIC 9(02).
000870     03  FILLER               PIC X(01) VALUE '/'.
000880     03  WRK-AAAA-POOL7600-E  PIC 9(04).
000890 01  WRK-DATA-POOL7600-E REDEFINES WRK-DATA-POOL7600-R
000900                              PIC X(10).
000910
000920*-------------------------------------------------------*
000930* F I M   D A   A R E A   D A   P O O L 7 6 0 0
000940*-------------------------------------------------------*
000950
000960
000970 77  FILLER                           PIC X(25) VALUE
000980             'INICIO DA WORKING-STORAGE'.
000990 77  WRK-49353                        PIC 9(05) VALUE  49353 COMP.
001000 77  WRK-49369                        PIC 9(05) VALUE  49369 COMP.
001010
001020 77  ACU-PALAVRAS                     PIC 9(02) VALUE  ZEROS.
001030
001050 77  WRK-INCONSIST                    PIC X(01) VALUE 'N'.
001060 77  WRK-FIM-PALAVRA                  PIC X(01) VALUE 'N'.
001070 77  WRK-OPCAO-DIGITADA               PIC X(01) VALUE ' '.
001080
001090 77  WRK-GU                           PIC X(04) VALUE 'GU'.
001100 77  WRK-ISRT                         PIC X(04) VALUE 'ISRT'.
001110 77  WRK-CHNG                         PIC X(04) VALUE 'CHNG'.
001120
001130
001140*--------------------------------*
001150* DEFINICAO DE AREAS DA POOL1000 *
001160*--------------------------------*
001170
001180 01  WRK-AREAS-POOL1000.
001190     05  WRK-FUNCAO                   PIC X(004)  VALUE SPACES.
001200     05  WRK-MENSAGEM.
001210         10  FILLER                   PIC X(0004).
001220         10  WRK-TRANCODE             PIC X(0008).
001230         10  FILLER                   PIC X(1988).
001240     05  WRK-MODNAME                  PIC X(008)  VALUE SPACES.
001260     05  WRK-USUARIO                  PIC 9(007)  VALUE ZEROS.
001270     05  WRK-ATUAL-OPCAO              PIC 9(002)  VALUE ZEROS.
001280     05  WRK-DEPTO                    PIC 9(006)  VALUE ZEROS.
001290
-INC POL7100C
001310
001320     EJECT
001330*------------------*
001340* AREAS AUXILIARES *
001350*------------------*
001360
001370 01  TABELA1.
001380     05  TAB-NOME1               PIC  X(35).
001390     05  FILLER                  REDEFINES
001400         TAB-NOME1       OCCURS  35  TIMES  INDEXED BY IND-LETRA1.
001410         10  TAB-LETRA1          PIC  X(01).
001420
001430 01  TABELA2.
001440     05  TAB-NOME2               PIC  X(35).
001450     05  FILLER                  REDEFINES
001470         10  TAB-LETRA2          PIC  X(01).
001480
001490 01  WRK-AUXILIAR.
001500     05  WRK-NRO-ESTABELECIMENTO.
001510         10  WRK-PREFEST         PIC  X(04).
001520         10  WRK-BCOEST          PIC  X(03).
001530         10  WRK-NUMEST          PIC  X(06).
001540     05  WRK-AGENCIA-CONTA.
001550         10  WRK-AGENC           PIC  X(04).
001560         10  WRK-CONTA           PIC  X(07).
001570     05  WRK-NOME                PIC  X(35).
001580     05  WRK-CGC-CPF.
001590         10  WRK-CGCCPF          PIC  X(09).
001600         10  WRK-FILIAL          PIC  X(04).
001610         10  WRK-DIGITO          PIC  X(02).
001620
001630     EJECT
001640*--------------------------------*
001650* DEFINICAO DE AREAS DA POOL0110 *
001660*--------------------------------*
7C2511*01  WRK-0110-CGCCPF.
7C2511*    05  WRK-0110-NUMERO         PIC  9(09).
7C2511*    05  WRK-0110-FILIAL         PIC  9(05).
001710
7C2511 01 WRK-BRAD2000                 PIC  X(08)  VALUE 'BRAD2000'.
7C2511 01 WRK-AREA-BRAD2000.
7C2511             05  WRK-CAD-CPFCGC.
7C2511             10  WRK-CAD-NUMERO         PIC X(09)           VALUE SPACES.
7C2511             10  WRK-CAD-FILIAL         PIC X(04)           VALUE SPACES.
7C2511             77  WRK-CAD-CONTROLE            PIC 9(02)           VALUE ZER

7C2511*01  WRK-0110-CONTROLE           PIC  X(02).
001730
001740     EJECT
001750*----------------------------------------------------------------*
001760*    TELA BVVET170            - INPUT                            *
001770*----------------------------------------------------------------*
001780
001790 01  INPUT-T170.
001800     05  FILLER                       PIC  X(04).
001810     05  INP-TRANCODE-T170            PIC  X(14).
001820     05  INP-PFK-T170                 PIC  X(01).
001830     05  INP-SENHA-T170               PIC  X(37).
001840     05  INP-NOPCAO-T170              PIC  X(02).
001850     05  INP-PREFEST-T170             PIC  X(04).
001860     05  INP-BCOEST-T170              PIC  X(03).
001870     05  INP-NUMEST-T170              PIC  X(06).
001890     05  INP-CONTA-T170               PIC  X(07).
001900     05  INP-NOMEST-T170              PIC  X(35).
001910     05  INP-CGCCPF-T170              PIC  X(09).
001920     05  INP-FILIAL-T170              PIC  X(04).
001930     05  INP-DIGITO-T170              PIC  X(02).
001940
001950     EJECT
001960*----------------------------------------------------------------*
001970*    TELA BVVET170            - OUTPUT                           *
001980*----------------------------------------------------------------*
001990
002000 01  OUTPUT-T170.
002010     05  OUT-LL-T170                  PIC S9(04) COMP VALUE +226.
002020     05  OUT-ZZ-T170                  PIC S9(04) COMP VALUE  0.
002030     05  OUT-DADOS-T170.
002040         10  OUT-SENHA-T170           PIC  X(37).
002050         10  OUT-NOPCAO-ATTR-T170     PIC  9(04) COMP.
002060         10  OUT-NOPCAO-T170          PIC  X(02).
002070         10  OUT-DATA-T170            PIC  X(10).
002080         10  OUT-PREFEST-ATTR-T170    PIC  9(04) COMP.
002100         10  OUT-BCOEST-ATTR-T170     PIC  9(04) COMP.
002110         10  OUT-BCOEST-T170          PIC  X(03).
002120         10  OUT-NUMEST-ATTR-T170     PIC  9(04) COMP.
002130         10  OUT-NUMEST-T170          PIC  X(06).
002140         10  OUT-AGENC-ATTR-T170      PIC  9(04) COMP.
002150         10  OUT-AGENC-T170           PIC  X(04).
002160         10  OUT-AGENC-T170-R REDEFINES OUT-AGENC-T170 PIC 9(04).
002170         10  OUT-CONTA-ATTR-T170      PIC  9(04) COMP.
002180         10  OUT-CONTA-T170           PIC  X(07).
002190         10  OUT-NOMEST-ATTR-T170     PIC  9(04) COMP.
002200         10  OUT-NOMEST-T170          PIC  X(35).
002210         10  OUT-CGCCPF-ATTR-T170     PIC  9(04) COMP.
002220         10  OUT-CGCCPF-T170          PIC  X(09).
002230         10  OUT-FILIAL-ATTR-T170     PIC  9(04) COMP.
002240         10  OUT-FILIAL-T170          PIC  X(04).
002250         10  OUT-DIGITO-ATTR-T170     PIC  9(04) COMP.
002260         10  OUT-DIGITO-T170          PIC  X(02).
002270         10  OUT-MENS-T170            PIC  X(79).
002280
002290     EJECT
002310*    AREA DA POOL0660 COM MASCARA DA TELA BVVET170 - SEGMENTO 00 *
002320*----------------------------------------------------------------*
002330
002340 01  660-BVVET170.
002350     05  660-LL-AREA-T170             PIC  9(04) COMP VALUE 0030.
002360     05  660-LL-MENSAGEM-T170         PIC  9(04) COMP VALUE 0226.
002370     05  660-SENHA-T170               PIC  9(04) COMP VALUE 0037.
002380     05  660-NOPCAO-T170              PIC  9(04) COMP VALUE 3004.
002390     05  660-DATA-T170                PIC  9(04) COMP VALUE 0010.
002400     05  660-PREFEST-T170             PIC  9(04) COMP VALUE 2006.
002410     05  660-BCOEST-T170              PIC  9(04) COMP VALUE 2005.
002420     05  660-NUMEST-T170              PIC  9(04) COMP VALUE 2008.
002430     05  660-AGENC-T170               PIC  9(04) COMP VALUE 2006.
002440     05  660-CONTA-T170               PIC  9(04) COMP VALUE 2009.
002450     05  660-NOMEST-T170              PIC  9(04) COMP VALUE 2037.
002460     05  660-CGCCPF-T170              PIC  9(04) COMP VALUE 2011.
002470     05  660-FILIAL-T170              PIC  9(04) COMP VALUE 2006.
002480     05  660-DIGITO-T170              PIC  9(04) COMP VALUE 2004.
002490     05  660-MENS-T170                PIC  9(04) COMP VALUE 0079.
002500
002520*----------------------------------------------------------------*
002530*    AREA DE COMUNICACAO                                         *
002540*----------------------------------------------------------------*
002550
002560 01  COMU-AREA.
002570     05  COMU-LL                      PIC S9(04) COMP VALUE +107.
002580     05  COMU-ZZ                      PIC S9(04) COMP VALUE  0.
002590     05  COMU-TRANCODE                PIC  X(15).
002600     05  COMU-SENHA                   PIC  X(37).
002610     05  COMU-OPCAO                   PIC  X(02).
002620     05  COMU-CHAVE                   PIC  X(35).
002630     05  COMU-171-NOME                REDEFINES
002640         COMU-CHAVE                   PIC  X(35).
002650     05  COMU-172-CGCCPF              REDEFINES
002660         COMU-CHAVE.
7C2511*        10  COMU-172-NUMERO          PIC  9(09).
7C2511         10  COMU-172-NUMERO          PIC  X(09).
7C2511*        10  COMU-172-FILIAL          PIC  9(04).
7C2511         10  COMU-172-FILIAL          PIC  X(04).
7C2511*        10  COMU-172-DIGITO          PIC  9(02).
7C2511         10  COMU-172-DIGITO          PIC  X(02).
002700         10  FILLER                   PIC  X(20).
002710     05  COMU-173-AGECON              REDEFINES
002730         10  COMU-173-AGENCIA         PIC  9(05).
002740         10  COMU-173-CONTA           PIC  9(07).
002750         10  FILLER                   PIC  X(23).
002760     05  COMU-FLAG                    PIC  X(01).
002770     05  COMU-174-NUMESTAB.
002780         10  COMU-174-PREFEST         PIC  9(04).
002790         10  COMU-174-BCOEST          PIC  9(03).
002800         10  COMU-174-NUMEST          PIC  9(06).
002810
002820     EJECT
002830*--------------------------------------------------------------*
002840* TEXTO PARA ERRO DOS MODULOS POOL0660                         *
002850*--------------------------------------------------------------*
002860
002870 01  WRK-TEXT-TELA-ERRO.
002880     05  WRK-ERR-MSG        PIC X(17)   VALUE 'ERRO NO ACESSO A'.
002890     05  WRK-ERR-MODULO     PIC X(08).
002900     05  FILLER             PIC X(08)   VALUE ' - TELA '.
002910     05  WRK-ERR-TELA       PIC X(08).
002920     05  FILLER             PIC X(11)   VALUE '  STATUS ='.
002940     05  FILLER             PIC X(10)   VALUE '  LOCAL ='.
002950     05  WRK-ERR-LOCAL      PIC 9(04).
002960
002970     EJECT
002980*--------------------------------------------------------------*
002990* MENSAGENS PARA SEREM UTILIZADAS NO PROGRAMA                  *
003000*--------------------------------------------------------------*
003010
003020 01  WRK-MENSAGENS.
003030     05  WRK-MSG01.
003040         10  FILLER                   PIC X(70)  VALUE
003050         'PF INVALIDA'.
003060     05  WRK-MSG02.
003070         10  FILLER                   PIC X(70)  VALUE
003080         'DIGITE APENAS UM DOS CAMPOS PARA PESQUISA'.
003090     05  WRK-MSG03.
003100         10  FILLER                   PIC X(70)  VALUE
003110         'ESCOLHA UMA OPCAO PARA PESQUISA E TECLE <ENTER>'.
003120     05  WRK-MSG04.
003130         10  FILLER                   PIC X(70)  VALUE
003150     05  WRK-MSG05.
003160         10  FILLER                   PIC X(70)  VALUE
003170         'NAO HA ESTABELECIMENTO CADASTRADO PARA ESTA CHAVE'.
003180     05  WRK-MSG06.
003190         10  FILLER                   PIC X(70)  VALUE
003200         'CAMPO BRILHANTE INVALIDO'.
003210     05  WRK-MSG07.
003220         10  FILLER                   PIC X(70)  VALUE
003230         'NAO INFORMAR "E", "OU", "DA", "DAS", "DO" OU "DE" '.
003240     05  WRK-MSG08.
003250         10  FILLER                   PIC X(70)  VALUE
003260         'PESQUISA PERMITIDA PARA NO MAXIMO 03 NOMES'.
003270     05  WRK-MSG09.
003280         10  FILLER                   PIC X(70)  VALUE
003290         'USUARIO NAO AUTORIZADO PARA ESTA PESQUISA'.
003300     05  WRK-MSG10.
003310         10  FILLER                   PIC X(70)  VALUE
003320         'USUARIO NAO ENCONTROU ARGUMENTO PARA PESQUISA'.
003330
003340     EJECT
003360*---------------*
003370
003380 01  IO-PCB.
003390     05  IO-LTERM.
003400         10  FILLER              PIC X(001).
003410         10  IO-LTERM2           PIC X(007).
003420     05  FILLER                  PIC X(002).
003430     05  IO-STATUS               PIC X(002).
003440     05  FILLER                  PIC X(012).
003450     05  IO-MODNAME              PIC X(008).
003460
003470 01  ALT-PCB.
003480     05  ALT-LTERM               PIC X(008).
003490     05  FILLER                  PIC X(002).
003500     05  ALT-STATUS              PIC X(002).
003510
003520     EJECT
003530*********************
003540 PROCEDURE DIVISION.
003550*********************
003570*---------------------*
003580 00000-INICIO SECTION.
003590*---------------------*
003600
003610     ENTRY       'DLITCBL'       USING     IO-PCB  ALT-PCB.
003620     MOVE         WRK-GU            TO     WRK-FUNCAO.
003630
003640 00000-010-POOL1000.
003650*------------------*
003660
003670     CALL    'POOL5000'      USING     WRK-FUNCAO
003680                                       IO-PCB
003690                                       ALT-PCB
003700                                       WRK-MENSAGEM
003710                                       WRK-MODNAME
003720                                       WRK-NOVA-OPCAO
003730                                       WRK-USUARIO
003740                                       WRK-ATUAL-OPCAO
003750                                       WRK-DEPTO.
003760
003780          GOBACK.
003790
003800     MOVE   LOW-VALUES           TO    INPUT-T170
003810                                       OUTPUT-T170
003820                                       COMU-AREA.
003830
003840     MOVE   SPACES               TO    OUT-DADOS-T170.
003850
003860     MOVE   +226                 TO    OUT-LL-T170.
003870     MOVE   +107                 TO    COMU-LL.
003880
003890     MOVE   'N'                  TO    WRK-INCONSIST
003900                                       WRK-LETRA
003910                                       WRK-FIM-PALAVRA.
003920
003930     MOVE   SPACES               TO    WRK-OPCAO-DIGITADA.
003940
003950     MOVE   SPACES               TO    TABELA1  TABELA2
003960                                       WRK-AUXILIAR.
003970
003990
004000     MOVE   SPACES               TO    WRK-FUNCAO.
004010
004020     MOVE   WRK-MENSAGEM         TO    INPUT-T170.
004030
004040     PERFORM     10000-ROTINA-PRINCIPAL.
004050
004060     GO          TO  00000-010-POOL1000.
004070
004080 00000-99-FIM.  EXIT.
004090
004100     EJECT
004110*-------------------------------*
004120 10000-ROTINA-PRINCIPAL SECTION.
004130*-------------------------------*
004140
004150     IF     WRK-TRANCODE    NOT   EQUAL   'BVVE0170'
004160            MOVE   WRK-MENSAGEM      TO    COMU-AREA
004170            MOVE   INP-SENHA-T170    TO    OUT-SENHA-T170
004180            IF     COMU-FLAG      EQUAL   '1'
004200                   MOVE    WRK-MSG05 TO    OUT-MENS-T170
004210            ELSE
004220                   IF  COMU-FLAG     EQUAL   '2'
004230                       PERFORM     20000-RECEBER-CONTROLE
004240                       MOVE WRK-MSG09  TO    OUT-MENS-T170
004250                   ELSE
004260                       IF  COMU-FLAG    EQUAL   '3'
004270                           PERFORM     20000-RECEBER-CONTROLE
004280                           MOVE WRK-MSG10  TO    OUT-MENS-T170
004290                       ELSE
004300                           MOVE WRK-MSG03  TO    OUT-MENS-T170
004310     ELSE
004320            PERFORM     30000-AJUSTAR-CAMPOS
004330            PERFORM     40000-VERIFICAR-PFS.
004340
004350     IF  WRK-FUNCAO               NOT =    'CHNG'
004360         PERFORM        50000-FORMATAR-SAIDA.
004370
004380 10000-99-FIM.  EXIT.
004390
004410*------------------------------*
004420 20000-RECEBER-CONTROLE SECTION.
004430*------------------------------*
004440
004450     IF     WRK-TRANCODE          EQUAL   'BVVE0171'
004460            MOVE    COMU-171-NOME    TO    OUT-NOMEST-T170
004470     ELSE
004480     IF     WRK-TRANCODE          EQUAL   'BVVE0172'
004490            MOVE    COMU-172-NUMERO  TO    OUT-CGCCPF-T170
004500            MOVE    COMU-172-FILIAL  TO    OUT-FILIAL-T170
004510            MOVE    COMU-172-DIGITO  TO    OUT-DIGITO-T170
004520     ELSE
004530     IF     WRK-TRANCODE          EQUAL   'BVVE0173'
004540**          MOVE    COMU-173-AGENCIA TO    OUT-AGENC-T170
004550            MOVE    COMU-173-AGENCIA TO    OUT-AGENC-T170-R
004560            MOVE    COMU-173-CONTA   TO    OUT-CONTA-T170
004570     ELSE
004580     IF     WRK-TRANCODE          EQUAL   'BVVE0174'
004590            MOVE    COMU-174-PREFEST TO    OUT-PREFEST-T170
004600            MOVE    COMU-174-BCOEST  TO    OUT-BCOEST-T170
004620
004630
004640 20000-99-FIM.  EXIT.
004650
004660     EJECT
004670*-----------------------------*
004680 30000-AJUSTAR-CAMPOS SECTION.
004690*-----------------------------*
004700
004710     IF     INP-PREFEST-T170   NOT EQUAL SPACES
004720            INSPECT     INP-PREFEST-T170 REPLACING  LEADING
004730                                         SPACES BY  ZEROS.
004740
004750     IF     INP-BCOEST-T170    NOT EQUAL SPACES
004760            INSPECT      INP-BCOEST-T170 REPLACING  LEADING
004770                                         SPACES BY  ZEROS.
004780
004790     IF     INP-NUMEST-T170    NOT EQUAL SPACES
004800            INSPECT      INP-NUMEST-T170 REPLACING  LEADING
004810                                         SPACES BY  ZEROS.
004830
004840
004850     IF     INP-CGCCPF-T170    NOT EQUAL SPACES
004860            INSPECT      INP-CGCCPF-T170 REPLACING  LEADING
004870                                         SPACES BY  ZEROS.
004880     IF     INP-FILIAL-T170    NOT EQUAL SPACES
004890            INSPECT      INP-FILIAL-T170 REPLACING  LEADING
004900                                         SPACES BY  ZEROS.
004910     IF     INP-DIGITO-T170    NOT EQUAL SPACES
004920            INSPECT      INP-DIGITO-T170 REPLACING  LEADING
004930                                         SPACES BY  ZEROS.
004940
004950
004960
004970     IF     INP-AGENC-T170     NOT EQUAL SPACES
004980            INSPECT       INP-AGENC-T170 REPLACING  LEADING
004990                                         SPACES BY  ZEROS.
005000     IF     INP-CONTA-T170     NOT EQUAL SPACES
005010            INSPECT       INP-CONTA-T170 REPLACING  LEADING
005020                                         SPACES BY  ZEROS.
005040 30000-99-FIM.  EXIT.
005050
005060     EJECT
005070*----------------------------*
005080 40000-VERIFICAR-PFS SECTION.
005090*----------------------------*
005100
005110* TRATAR PF INVALIDA
005120
005130     IF     INP-PFK-T170           EQUAL    '*'
005140            PERFORM     41000-MANTER-TELA
005150            MOVE     WRK-MSG01        TO    OUT-MENS-T170
005160            GO   TO  40000-99-FIM.
005170
005180* TRATAR HELP6100
005190
005200     IF     INP-PFK-T170         EQUAL    'H'
005210            PERFORM     41000-MANTER-TELA
005220            MOVE    WRK-MSG03        TO    OUT-MENS-T170
005230            GO   TO  40000-99-FIM.
005250
005260* TRATAR ENTER
005270
005280     PERFORM     42000-TRATAR-CONSISTENCIA.
005290
005300     IF     WRK-INCONSIST         EQUAL   'S'
005310            PERFORM     41000-MANTER-TELA
005320     ELSE
005330            PERFORM     43000-PASSAR-CONTROLE.
005340
005350 40000-99-FIM.  EXIT.
005360
005370     EJECT
005380*--------------------------*
005390 41000-MANTER-TELA SECTION.
005400*--------------------------*
005410
005420     MOVE    INP-SENHA-T170      TO    OUT-SENHA-T170.
005430     MOVE    INP-PREFEST-T170    TO    OUT-PREFEST-T170.
005440     MOVE    INP-BCOEST-T170     TO    OUT-BCOEST-T170.
005460     MOVE    INP-AGENC-T170      TO    OUT-AGENC-T170.
005470     MOVE    INP-CONTA-T170      TO    OUT-CONTA-T170.
005480     MOVE    INP-NOMEST-T170     TO    OUT-NOMEST-T170.
005490     MOVE    INP-CGCCPF-T170     TO    OUT-CGCCPF-T170.
005500     MOVE    INP-FILIAL-T170     TO    OUT-FILIAL-T170.
005510     MOVE    INP-DIGITO-T170     TO    OUT-DIGITO-T170.
005520
005530 41000-99-FIM.  EXIT.
005540
005550     EJECT
005560*---------------------------------*
005570 42000-TRATAR-CONSISTENCIA SECTION.
005580*---------------------------------*
005590
005600* CONSISTENCIA FISICA
005610
005620*****    VERIFICAR SE FOI DIGITADO ALGUMA OPCAO
005630
005640     IF     INP-PREFEST-T170      EQUAL    SPACES   AND
005650            INP-BCOEST-T170       EQUAL    SPACES   AND
005670            INP-AGENC-T170        EQUAL    SPACES   AND
005680            INP-CONTA-T170        EQUAL    SPACES   AND
005690            INP-NOMEST-T170       EQUAL    SPACES   AND
005700            INP-CGCCPF-T170       EQUAL    SPACES   AND
005710            INP-FILIAL-T170       EQUAL    SPACES   AND
005720            INP-DIGITO-T170       EQUAL    SPACES
005730            MOVE    'S'              TO    WRK-INCONSIST
005740            MOVE    WRK-MSG03        TO    OUT-MENS-T170
005750            GO  TO  42000-99-FIM.
005760
005770*****    VERIFICAR SE MAIS DE UMA OPCAO FOI DIGITADA
005780*****    CONSISTENCIA DE   CAMPOS
005790
005800     PERFORM     42100-CONSISTIR-CAMPOS.
005810
005820     IF     WRK-OPCAO-DIGITADA      NOT    EQUAL    '1'
005830            MOVE    'S'              TO    WRK-INCONSIST
005840            MOVE    WRK-MSG02        TO    OUT-MENS-T170
005850            GO  TO  42000-99-FIM.
005860
005880
005890
005900     EJECT
005910*-------------------------------*
005920 42100-CONSISTIR-CAMPOS SECTION.
005930*-------------------------------*
005940
005950     MOVE    INP-PREFEST-T170    TO    WRK-PREFEST.
005960     MOVE    INP-BCOEST-T170     TO    WRK-BCOEST.
005970     MOVE    INP-NUMEST-T170     TO    WRK-NUMEST.
005980     MOVE    INP-AGENC-T170      TO    WRK-AGENC.
005990     MOVE    INP-CONTA-T170      TO    WRK-CONTA.
006000     MOVE    INP-NOMEST-T170     TO    WRK-NOME.
006010     MOVE    INP-CGCCPF-T170     TO    WRK-CGCCPF.
006020     MOVE    INP-FILIAL-T170     TO    WRK-FILIAL.
006030     MOVE    INP-DIGITO-T170     TO    WRK-DIGITO.
006040
006050     IF     WRK-NRO-ESTABELECIMENTO   NOT EQUAL SPACES AND
006060            WRK-AGENCIA-CONTA             EQUAL SPACES AND
006070            WRK-NOME                      EQUAL SPACES AND
006090            MOVE    '1'          TO   WRK-OPCAO-DIGITADA
006100            PERFORM     42110-CONSISTIR-NRO-ESTABELEC
006110            GO   TO  42100-99-FIM.
006120
006130     IF     WRK-NRO-ESTABELECIMENTO       EQUAL SPACES AND
006140            WRK-AGENCIA-CONTA         NOT EQUAL SPACES AND
006150            WRK-NOME                      EQUAL SPACES AND
006160            WRK-CGC-CPF                   EQUAL SPACES
006170            MOVE    '1'          TO   WRK-OPCAO-DIGITADA
006180            PERFORM     42120-CONSISTIR-AGENCIA-CTA
006190            GO   TO  42100-99-FIM.
006200
006210     IF     WRK-NRO-ESTABELECIMENTO       EQUAL SPACES AND
006220            WRK-AGENCIA-CONTA             EQUAL SPACES AND
006230            WRK-NOME                  NOT EQUAL SPACES AND
006240            WRK-CGC-CPF                   EQUAL SPACES
006250            MOVE    '1'          TO   WRK-OPCAO-DIGITADA
006260            PERFORM     42130-CONSISTIR-NOME
006270            GO   TO  42100-99-FIM.
006280
006300            WRK-AGENCIA-CONTA             EQUAL SPACES AND
006310            WRK-NOME                      EQUAL SPACES AND
006320            WRK-CGC-CPF               NOT EQUAL SPACES
006330            MOVE    '1'         TO    WRK-OPCAO-DIGITADA
006340            PERFORM  42140-CONSISTIR-CGC-CPF.
006350
006360 42100-99-FIM.  EXIT.
006370
006380     EJECT
006390*--------------------------------------*
006400 42110-CONSISTIR-NRO-ESTABELEC SECTION.
006410*--------------------------------------*
006420
006430     IF     INP-PREFEST-T170     IS NOT    NUMERIC    OR
006440            INP-PREFEST-T170      EQUAL    ZEROS
006450            MOVE    WRK-49369        TO    OUT-PREFEST-ATTR-T170
006460            MOVE    WRK-MSG06        TO    OUT-MENS-T170
006470            MOVE    'S'              TO    WRK-INCONSIST
006480            GO  TO  42110-99-FIM.
006490
006510**          INP-BCOEST-T170       EQUAL    ZEROS
006520     IF     INP-BCOEST-T170      IS NOT    NUMERIC
006530            MOVE    WRK-49369        TO    OUT-BCOEST-ATTR-T170
006540            MOVE    WRK-MSG06        TO    OUT-MENS-T170
006550            MOVE    'S'              TO    WRK-INCONSIST
006560            GO  TO  42110-99-FIM.
006570
006580     IF     INP-NUMEST-T170      IS NOT    NUMERIC    OR
006590            INP-NUMEST-T170       EQUAL    ZEROS
006600            MOVE    WRK-49369        TO    OUT-NUMEST-ATTR-T170
006610            MOVE    WRK-MSG06        TO    OUT-MENS-T170
006620            MOVE    'S'              TO    WRK-INCONSIST
006630            GO  TO  42110-99-FIM.
006640
006650
006660     IF     WRK-INCONSIST      NOT EQUAL    'S'
006670            MOVE    INP-PREFEST-T170 TO    COMU-174-PREFEST
006680            MOVE    INP-BCOEST-T170  TO    COMU-174-BCOEST
006690            MOVE    INP-NUMEST-T170  TO    COMU-174-NUMEST
006700            MOVE    'BVVE0174'       TO    WRK-MODNAME.
006720 42110-99-FIM.  EXIT.
006730
006740
006750     EJECT
006760*-------------------------------------*
006770 42120-CONSISTIR-AGENCIA-CTA  SECTION.
006780*-------------------------------------*
006790
006800     IF     INP-AGENC-T170       IS NOT    NUMERIC    OR
006810            INP-AGENC-T170        EQUAL    ZEROS
006820            MOVE    WRK-49369        TO    OUT-AGENC-ATTR-T170
006830            MOVE    WRK-MSG06        TO    OUT-MENS-T170
006840            MOVE    'S'              TO    WRK-INCONSIST
006850            GO  TO  42120-99-FIM.
006860
006870     IF     INP-CONTA-T170       IS NOT    NUMERIC    OR
006880            INP-CONTA-T170        EQUAL    ZEROS
006890            MOVE    WRK-49369       TO     OUT-CONTA-ATTR-T170
006900            MOVE    WRK-MSG06       TO     OUT-MENS-T170
006910            MOVE    'S'             TO     WRK-INCONSIST
006930
006940
006950     IF     WRK-INCONSIST      NOT EQUAL   'S'
006960            MOVE    INP-AGENC-T170    TO   COMU-173-AGENCIA
006970            MOVE    INP-CONTA-T170    TO   COMU-173-CONTA
006980            MOVE    'BVVE0173'        TO   WRK-MODNAME.
006990
007000 42120-99-FIM.  EXIT.
007010
007020     EJECT
007030*------------------------------*
007040 42130-CONSISTIR-NOME  SECTION.
007050*------------------------------*
007060
007070     MOVE   INP-NOMEST-T170     TO   TAB-NOME1
007080     IF     TAB-NOME1        EQUAL    'DA'  OR
007090            TAB-NOME1        EQUAL    'DE'  OR
007100            TAB-NOME1        EQUAL    'DO'  OR
007110            TAB-NOME1        EQUAL    'DAS' OR
007120            TAB-NOME1        EQUAL    'E'   OR
007140            MOVE    WRK-MSG07   TO    OUT-MENS-T170
007150            MOVE    WRK-49353   TO    OUT-NOMEST-ATTR-T170
007160            MOVE    'S'         TO    WRK-INCONSIST
007170            GO TO   42130-99-FIM.
007180
007190     MOVE    INP-NOMEST-T170     TO   TAB-NOME1
007200     SET     IND-LETRA1          TO   1.
007210
007220     PERFORM     42131-TRATAR-NOME                     VARYING
007230                        IND-LETRA1       FROM  1 BY 1    UNTIL
007240                        IND-LETRA1    GREATER  35           OR
007250                        WRK-INCONSIST   EQUAL 'S'.
007260
007270     IF     ACU-PALAVRAS    GREATER   3
007280            MOVE    WRK-MSG08    TO   OUT-MENS-T170
007290            MOVE    'S'          TO   WRK-INCONSIST.
007300
007310     IF     WRK-INCONSIST          EQUAL   'S'
007320            MOVE    WRK-49353         TO   OUT-NOMEST-ATTR-T170
007330     ELSE
007350            MOVE    'BVVE0171'        TO   WRK-MODNAME.
007360
007370 42130-99-FIM.  EXIT.
007380
007390     EJECT
007400*---------------------------*
007410 42131-TRATAR-NOME  SECTION.
007420*---------------------------*
007430
007440     MOVE    'N'                 TO   WRK-LETRA.
007450     PERFORM     42132-ELIMINAR-ESPACO                   UNTIL
007460                             WRK-LETRA    EQUAL    'S'      OR
007470                             IND-LETRA1   GREATER  35.
007480
007490     IF     IND-LETRA1     GREATER   35
007500            GO  TO  42131-99-FIM.
007510
007520     MOVE    ' '                 TO   TAB-NOME2.
007530     MOVE    'N'                 TO   WRK-FIM-PALAVRA.
007540     PERFORM     42133-MONTAR-PALAVRA                  VARYING
007560                           WRK-FIM-PALAVRA     EQUAL 'S'    OR
007570                           IND-LETRA1        GREATER  35.
007580
007590**   IF     TAB-NOME2        EQUAL    'DA'  OR
007600**          TAB-NOME2        EQUAL    'DE'  OR
007610**          TAB-NOME2        EQUAL    'DO'  OR
007620**          TAB-NOME2        EQUAL    'DAS' OR
007630**          TAB-NOME2        EQUAL    'E'   OR
007640**          TAB-NOME2        EQUAL    'OU'
007650**          MOVE     WRK-MSG07  TO    OUT-MENS-T170
007660**   ELSE
007670     ADD       1         TO    ACU-PALAVRAS.
007680
007690 42131-99-FIM.  EXIT.
007700
007710
007720     EJECT
007730*---------------------------------*
007740 42132-ELIMINAR-ESPACO    SECTION.
007750*---------------------------------*
007770     IF  TAB-LETRA1(IND-LETRA1)  EQUAL SPACES
007780         SET  IND-LETRA1  UP BY 1
007790     ELSE
007800         MOVE  'S'               TO WRK-LETRA.
007810
007820 42132-99-FIM.  EXIT.
007830
007840
007850     EJECT
007860*---------------------------------*
007870 42133-MONTAR-PALAVRA     SECTION.
007880*---------------------------------*
007890
007900     IF    TAB-LETRA1(IND-LETRA1) EQUAL  SPACES
007910           MOVE     'S'              TO  WRK-FIM-PALAVRA
007920           GO        TO    42133-99-FIM.
007930
007940     MOVE  TAB-LETRA1(IND-LETRA1) TO TAB-LETRA2(IND-LETRA2).
007950
007960     SET   IND-LETRA1   UP BY 1.
007980 42133-99-FIM.  EXIT.
007990
008000
008010     EJECT
008020*---------------------------------*
008030 42140-CONSISTIR-CGC-CPF  SECTION.
008040*---------------------------------*
008050
008060     IF     INP-CGCCPF-T170      IS NOT    NUMERIC
008070            MOVE     WRK-49369       TO    OUT-CGCCPF-ATTR-T170
008080            MOVE     WRK-MSG06       TO    OUT-MENS-T170
008090            MOVE     'S'             TO    WRK-INCONSIST
008100            GO  TO  42140-99-FIM.
008110
008120     IF     INP-FILIAL-T170      IS NOT    NUMERIC
008130            MOVE     WRK-49369       TO    OUT-FILIAL-ATTR-T170
008140            MOVE     WRK-MSG06       TO    OUT-MENS-T170
008150            MOVE     'S'             TO    WRK-INCONSIST
008160            GO  TO  42140-99-FIM.
008170
008190            MOVE     WRK-49369       TO    OUT-DIGITO-ATTR-T170
008200            MOVE     WRK-MSG06       TO    OUT-MENS-T170
008210            MOVE     'S'             TO    WRK-INCONSIST
008220            GO  TO  42140-99-FIM.
008230
7C2511*    MOVE    INP-CGCCPF-T170         TO    WRK-0110-NUMERO.
7C2511*    MOVE    INP-FILIAL-T170         TO    WRK-0110-FILIAL.
7C2511*    CALL    'POOL0110'           USING    WRK-0110-CGCCPF
7C2511*                                      WRK-0110-CONTROLE.
7C2511             MOVE ZEROS          TO WRK-CAD-CONTROLE.
7C2511             MOVE INP-CGCCPF-T170 TO WRK-CAD-NUMERO.
7C2511             MOVE INP-FILIAL-T170 TO WRK-CAD-FILIAL.
7C2511             CALL WRK-BRAD2000     USING WRK-CAD-CPFCGC
7C2511                        WRK-CAD-CONTROLE.
7C2511             IF RETURN-CODE NOT EQUAL ZEROS
7C2511* CASO OCORRA ALGUM ERRO UTILIZAR O TRATAMENTO DE ERRO PADRAO DO PROGRAMA
7C2511             END-IF.
008300     IF     INP-DIGITO-T170   NOT EQUAL    WRK-CAD-CONTROLE
008310            MOVE     WRK-49369       TO    OUT-DIGITO-ATTR-T170
008320            MOVE     WRK-MSG04       TO    OUT-MENS-T170
008330            MOVE     'S'             TO    WRK-INCONSIST
008340            GO  TO  42140-99-FIM.
008350
008360     IF     WRK-INCONSIST     NOT EQUAL    'S'
7C2511*           Movimentos para COMU-172 (agora PIC X) preservando zeros � esq
008370            MOVE     INP-CGCCPF-T170 TO    COMU-172-NUMERO
008380            MOVE     INP-FILIAL-T170 TO    COMU-172-FILIAL
008400            MOVE     'BVVE0172'      TO    WRK-MODNAME.
008410
008420 42140-99-FIM.  EXIT.
008430
008440     EJECT
008450*------------------------------*
008460 43000-PASSAR-CONTROLE SECTION.
008470*------------------------------*
008480
008490     MOVE    +107                TO   COMU-LL.
008500     MOVE    INP-SENHA-T170      TO   COMU-SENHA.
008510     MOVE    'BVVE0170'          TO   COMU-TRANCODE.
008520     MOVE    WRK-CHNG            TO   WRK-FUNCAO.
008530     MOVE    COMU-AREA           TO   WRK-MENSAGEM.
008540
008550 43000-99-FIM.  EXIT.
008560
008570     EJECT
008580*-----------------------------*
008590 50000-FORMATAR-SAIDA SECTION.
008610
008620     CALL  'POOL7600'     USING    DATA-HORA.
008630
008640     MOVE  DT-AAAAMMDD          TO     WRK-DATA-POOL7600.
008650     MOVE  WRK-AAAA-POOL7600    TO     WRK-AAAA-POOL7600-E.
008660     MOVE  WRK-MM-POOL7600      TO     WRK-MM-POOL7600-E.
008670     MOVE  WRK-DD-POOL7600      TO     WRK-DD-POOL7600-E.
008680     MOVE  WRK-DATA-POOL7600-E  TO     OUT-DATA-T170.
008690
008700     CALL        'POOL0660'       USING      OUTPUT-T170
008710                                             660-BVVET170.
008720
008730     IF          RETURN-CODE  NOT EQUAL      ZEROS
008740         MOVE    'APL'               TO      ERR-TIPO-ACESSO
008750         MOVE    'POOL0660'          TO      WRK-ERR-MODULO
008760         MOVE    'BVVET170'          TO      WRK-ERR-TELA
008770         MOVE    RETURN-CODE         TO      WRK-ERR-STA
008780         MOVE    10                  TO      WRK-ERR-LOCAL
008790         MOVE    WRK-TEXT-TELA-ERRO  TO      ERR-TEXTO
008800         PERFORM 99999-ROTINA-ERRO.
008820     MOVE   'BVVET170'               TO      WRK-MODNAME.
008830     MOVE    WRK-ISRT                TO      WRK-FUNCAO.
008840     MOVE    OUTPUT-T170             TO      WRK-MENSAGEM.
008850
008860 50000-99-FIM. EXIT.
008870
008880     EJECT
008890*--------------------------*
008900 99999-ROTINA-ERRO SECTION.
008910*--------------------------*
008920
008930     MOVE   'SENHAS02'             TO       ERR-PGM.
008940     MOVE   'BVVE0170'             TO       ERR-MODULO.
008950     MOVE    WRK-DEPTO             TO       ERR-COD-DEPTO.
008960     MOVE    WRK-USUARIO           TO       ERR-COD-USER.
008970
008980     CALL    'POOL7100'         USING       IO-PCB  ERRO-AREA
008990                                            ALT-PCB.
009000     GOBACK.
009010
