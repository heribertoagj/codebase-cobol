      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT1838.
       AUTHOR. MICHELE JUNKES.
      *================================================================*
      *                    C P M   S I S T E M A S                     *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT1838                                     *
      *    PROGRAMADORA : MICHELE JUNKES                -  CPM/FPOLIS  *
      *    ANALISTA CPM : PEREIRA                       -  CPM/FPOLIS  *
      *    ANALISTA     : MARCELO CREMM - PROCWORK      -  GRUPO 50    *
      *    DATA         : 24/01/2007                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      GERA EXCEDENTE DE TARIFA E ISENTOS. PROGRAMA TAMBEM PREVE *
      *      RESTART-COMMIT COM MODULO NOVO CKRS0100 (PRODUTO APRESEN- *
      *      TADO PELO BRADESCO EM 12/01/2006).                        *
      *                                                                *
      *    ARQUIVOS     :                                              *
      *      DDNAME                                INCLUDE/BOOK        *
      *      COMPCLII                                I#GFCTVB          *
      *      EVCADSAI                                I#GFCTVA          *
      *      EXCED92O                                I#GFCTVA          *
      *      ISENT41O                                I#GFCTV4          *
310709*      ST18T41O                                I#GFCTV4          *
      *      ISENT92O                                I#GFCTVA          *
      *      EXCEDRES                                I#GFCTVA          *
      *      STGRPSRV                                I#GFCTVA          *
310709*      ST18T92O                                I#GFCTVA          *
      *      EVNBATEO                                I#GFCTVA          *
      *                                                                *
      *    BANCO DE DADOS :                                            *
      *     DB2                                                        *
      *      TABLE                                 INCLUDE/BOOK        *
      *      DB2PRD.PARM_DATA_PROCM                  GFCTB0A1          *
      *      DB2PRD.CTRL_PCOTE_MOVTO                 GFCTB034          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      POOL7100 - TRATAMENTO DE ERROS.                           *
      *      CKRS0100 - TRATAMENTO DE COMMIT-RESTART.                  *
      *                                                                *
      *================================================================*
      *                        A L T E R A C A O                       *
      *----------------------------------------------------------------*
      *                                                                *
      *    ANALISTA     : ALEXANDRE MUNIZ - PROCWORK                   *
      *    DATA         : 12/02/2009                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      IDENTIFICAR OS EVENTOS DE GRUPO DE SERVICO, GRAVANDO NUM  *
      *      ARQUIVO PARA POSTERIOR APURACAO DIARIA. RETIRADA DO ARQ.  *
      *      ST18T41O, MUDANCA DE NOME DO ARQUIVO DE ST18T92O PARA     *
      *      STGRPSRV.                                                 *
      *                                                                *
      *----------------------------------------------------------------*
      *                                                                *
      *    ANALISTA     : MARCUS VINICIUS - PROCWORK    - GRUPO 50     *
      *    DATA         : 10/03/2008                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      INCLUSAO DA PARTE REFERENTE A TRATAMENTO DE GRUPO DE      *
      *      SERVICO. TRATANDO TAMBEM OS EXCEDENTE POR RESTRICAO.      *
      *                                                                *
      *================================================================*
      *                        A L T E R A C A O                       *
      *----------------------------------------------------------------*
      *                                                                *
      *    ANALISTA     : VALERIA TORQUATO  PROCWORK                   *
      *    DATA         : 31/07/2009                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      IDENTIFICAR OS EVENTOS DE GRUPO DE SERVICO CUJA COBRANCA  *
      *      SEJA MENSAL CPER-COBR-COMPO = 1. OS REGISTROS COM CODIGO  *
      *      IGUAL A 3, COBRANCA  PELA TARIFA SEGUEM NA APURACAO       *
      *      DIARIA. INCLUSAO DOS ARQUIVOS PARA A COBRANCA MENSAL      *
      *      ST18T41O E ST18T92O QUE SERAO IDENTIFICADOS PELA SITUACAO *
      *      IGUAL A 18.                                               *
      *                                                                *
      *----------------------------------------------------------------*
151009*----------------------------------------------------------------*
151009*                                                                *
151009*    ANALISTA     : MARCUS VINICIUS   PROCWORK                   *
151009*    DATA         : 15/10/2009                                   *
151009*                                                                *
151009*    OBJETIVO     :                                              *
151009*      IDENTIFICAR OS EVENTOS DE TARIFA QUE POSSUAM UMA TARIFA   *
151009*      COMPONENTE COM CONTROLE DE UTILIZACAO POR QTDE DE         *
151009*      MOVIMENTOS.                                               *
151009*                                                                *
151009*----------------------------------------------------------------*
      *================================================================*
CAPG=I*================================================================*
.     *      PROGRAMADOR: PRISCILA BARBOSA- CAPGEMINI                 *
.     *      ANALISTA...: PRISCILA BARBOSA- CAPGEMINI                 *
.     *      DATA.......: 28/07/2015                                  *
.     *---------------------------------------------------------------*
.     *      OBJETIVO: INCLUIR A CHAMADA DO MODULO CKRS1000           *
.     *---------------------------------------------------------------*
.     *      MODULOS.: CKRS1000 - IDENTIFICAR DINAMICAMENTE QUAL O    *
.     *                           TIPO DE CONEXAO ESTA SENDO UTILIZADA*
.     *---------------------------------------------------------------*
.     *   ESTA MANUTENCAO ESTA IDENTIFICADA NO PROGRAMA COMO: CAPG=I  *
CAPG=I*===============================================================*
VI0117*----------------------------------------------------------------*
VI0117*                                                                *
VI0117*    ANALISTA     : MARCUS VINICIUS   INFOSERVER                 *
VI0117*    DATA         : JAN/2017                                     *
VI0117*                                                                *
VI0117*    OBJETIVO     :                                              *
VI0117*      VALORIZAR O CAMPO QUE CONTEM  PACOTE DE TARIFA.           *
VI0117*                                                                *
VI0117*----------------------------------------------------------------*
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT COMPCLII ASSIGN      TO UT-S-COMPCLII
                      FILE STATUS      IS WRK-FS-COMPCLII.

           SELECT EVCADSAI ASSIGN      TO UT-S-EVCADSAI
                      FILE STATUS      IS WRK-FS-EVCADSAI.

           SELECT EXCED92O ASSIGN      TO UT-S-EXCED92O
                      FILE STATUS      IS WRK-FS-EXCED92O.

           SELECT ISENT41O ASSIGN      TO UT-S-ISENT41O
                      FILE STATUS      IS WRK-FS-ISENT41O.

310709     SELECT ST18T41O ASSIGN      TO UT-S-ST18T41O
310709                FILE STATUS      IS WRK-FS-ST18T41O.

           SELECT ISENT92O ASSIGN      TO UT-S-ISENT92O
                      FILE STATUS      IS WRK-FS-ISENT92O.

           SELECT EXCEDRES ASSIGN      TO UT-S-EXCEDRES
                      FILE STATUS      IS WRK-FS-EXCEDRES.

           SELECT STGRPSRV ASSIGN      TO UT-S-STGRPSRV
                      FILE STATUS      IS WRK-FS-STGRPSRV.

310709     SELECT ST18T92O ASSIGN      TO UT-S-ST18T92O
310709                FILE STATUS      IS WRK-FS-ST18T92O.
310709
           SELECT EVNBATEO ASSIGN      TO UT-S-EVNBATEO
                      FILE STATUS      IS WRK-FS-EVNBATEO.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT  :  COMPONENTES DOS CLIENTES                          *
      *              ORG. SEQUENCIAL   -   LRECL = 0100                *
      *----------------------------------------------------------------*

       FD  COMPCLII
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-COMPCLII             PIC X(100).

      *----------------------------------------------------------------*
      *    INPUT  :  EVENTOS ADERIDOS                                  *
      *              ORG. SEQUENCIAL   -   LRECL = 0250                *
      *----------------------------------------------------------------*

       FD  EVCADSAI
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-EVCADSAI             PIC X(250).

      *----------------------------------------------------------------*
      *    OUTPUT :  EVENTOS EXCEDENTES DE TARIFA                      *
      *              ORG. SEQUENCIAL   -   LRECL = 0250                *
      *----------------------------------------------------------------*

       FD  EXCED92O
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-EXCED92O             PIC X(250).

      *----------------------------------------------------------------*
      *    OUTPUT :  EVENTOS ISENTOS DE TARIFA - TABELA 41             *
      *              ORG. SEQUENCIAL   -   LRECL = 0150                *
      *----------------------------------------------------------------*

       FD  ISENT41O
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-ISENT41O             PIC X(150).

310709*----------------------------------------------------------------*
310709*    OUTPUT :  EVENTOS ISENTOS DE TARIFA - TABELA 41             *
310709*              ORG. SEQUENCIAL   -   LRECL = 0150                *
310709*----------------------------------------------------------------*
310709
310709 FD  ST18T41O
310709     RECORDING MODE IS F
310709     LABEL RECORD IS STANDARD
310709     BLOCK CONTAINS 0 RECORDS.
310709
310709 01  FD-REG-ST18T41O             PIC X(150).

      *----------------------------------------------------------------*
      *    OUTPUT :  EVENTOS ISENTOS DE TARIFA - TABELA 92             *
      *              ORG. SEQUENCIAL   -   LRECL = 0250                *
      *----------------------------------------------------------------*

       FD  ISENT92O
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-ISENT92O             PIC X(250).

      *----------------------------------------------------------------*
      *    OUTPUT :  EVENTOS EXCEDENTES DE                             *
      *              RESTRICAO PARA TARIFAS DE GRUPO DE SERVICO        *
      *              ORG. SEQUENCIAL   -   LRECL = 0250                *
      *----------------------------------------------------------------*

       FD  EXCEDRES
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-EXCEDRES             PIC X(250).

      *----------------------------------------------------------------*
      *    OUTPUT :  EVENTOS ISENTOS COM SITUACAO 16 -  TABELA 92      *
      *              PARA TARIFAS DE GRUPO DE SERVICO                  *
      *              ORG. SEQUENCIAL   -   LRECL = 0250                *
      *----------------------------------------------------------------*

       FD  STGRPSRV
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-STGRPSRV             PIC X(250).

310709*----------------------------------------------------------------*
310709*    OUTPUT :  EVENTOS ISENTOS COM SITUACAO 18 -  TABELA 92      *
310709*              PARA TARIFAS DE GRUPO DE SERVICO                  *
310709*              ORG. SEQUENCIAL   -   LRECL = 0250                *
310709*----------------------------------------------------------------*
310709
310709 FD  ST18T92O
310709     RECORDING MODE IS F
310709     LABEL RECORD IS STANDARD
310709     BLOCK CONTAINS 0 RECORDS.
310709
310709 01  FD-REG-ST18T92O             PIC X(250).

      *----------------------------------------------------------------*
      *    OUTPUT :  ARQUIVO DE EVENTO SEM COMPONENTE.                 *
      *              ORG. SEQUENCIAL   -   LRECL = 0250                *
      *----------------------------------------------------------------*

       FD  EVNBATEO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-EVNBATEO             PIC X(250).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT1838  *'.
      *----------------------------------------------------------------*
       01  WRK-TIMESTAMP               PIC  X(026)         VALUE
           '0001-01-01-01.01.01.000001'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*         ACUMULADORES         *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05  ACU-LIDOS-COMPCLII      PIC 9(11) COMP-3    VALUE ZEROS.
           05  ACU-LIDOS-EVCADSAI      PIC 9(11) COMP-3    VALUE ZEROS.
           05  ACU-GRAVA-EXCED92O      PIC 9(11) COMP-3    VALUE ZEROS.
           05  ACU-GRAVA-ISENT41O      PIC 9(11) COMP-3    VALUE ZEROS.
310709     05  ACU-GRAVA-ST18T41O      PIC 9(11) COMP-3    VALUE ZEROS.
           05  ACU-GRAVA-ISENT92O      PIC 9(11) COMP-3    VALUE ZEROS.
           05  ACU-GRAVA-EXCEDRES      PIC 9(11) COMP-3    VALUE ZEROS.
           05  ACU-GRAVA-STGRPSRV      PIC 9(11) COMP-3    VALUE ZEROS.
310709     05  ACU-GRAVA-ST18T92O      PIC 9(11) COMP-3    VALUE ZEROS.
           05  ACU-GRAVA-EVNBATEO      PIC 9(11) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*           DE FILE-STATUS     *'.
      *----------------------------------------------------------------*

       01  WRK-AREA-FS.
           05  WRK-FS-COMPCLII         PIC X(02)           VALUE SPACES.
           05  WRK-FS-EVCADSAI         PIC X(02)           VALUE SPACES.
           05  WRK-FS-EXCED92O         PIC X(02)           VALUE SPACES.
           05  WRK-FS-ISENT41O         PIC X(02)           VALUE SPACES.
310709     05  WRK-FS-ST18T41O         PIC X(02)           VALUE SPACES.
           05  WRK-FS-ISENT92O         PIC X(02)           VALUE SPACES.
           05  WRK-FS-EXCEDRES         PIC X(02)           VALUE SPACES.
           05  WRK-FS-STGRPSRV         PIC X(02)           VALUE SPACES.
310709     05  WRK-FS-ST18T92O         PIC X(02)           VALUE SPACES.
           05  WRK-FS-EVNBATEO         PIC X(02)           VALUE SPACES.
           05  WRK-ABERTURA            PIC X(13)           VALUE
               ' NA ABERTURA '.
           05  WRK-LEITURA             PIC X(13)           VALUE
               ' NA  LEITURA '.
           05  WRK-GRAVACAO            PIC X(13)           VALUE
               ' NA GRAVACAO '.
           05  WRK-FECHAMENTO          PIC X(13)           VALUE
               'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     CHAVES DOS ARQUIVOS      *'.
      *----------------------------------------------------------------*

       01  WRK-CHAVE-EVENTO.
           05 WRK-EVENTO-AGENCIA       PIC 9(05)           VALUE ZEROS.
           05 WRK-EVENTO-CONTA         PIC 9(13)           VALUE ZEROS.
           05 WRK-EVENTO-COMPONENTE    PIC 9(05)           VALUE ZEROS.

151009 01  WRK-CHAVE-EVENTO-ANT.
151009     05 WRK-EVENTO-AGENC-ANT     PIC 9(05)           VALUE ZEROS.
151009     05 WRK-EVENTO-CONTA-ANT     PIC 9(13)           VALUE ZEROS.
151009     05 WRK-EVENTO-COMPO-ANT     PIC 9(05)           VALUE ZEROS.

       01  WRK-CHAVE-W-EVENTO.
           05 WRK-W-EVENTO-AGENCIA     PIC 9(05)           VALUE ZEROS.
           05 WRK-W-EVENTO-CONTA       PIC 9(13)           VALUE ZEROS.
           05 WRK-W-EVENTO-COMPONENTE  PIC 9(05)           VALUE ZEROS.

       01  WRK-CHAVE-COMPONENTE.
           05 WRK-COMPO-AGENCIA        PIC 9(05)           VALUE ZEROS.
           05 WRK-COMPO-CONTA          PIC 9(13)           VALUE ZEROS.
           05 WRK-COMPO-COMPONENTE     PIC 9(05)           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-BATCH                PIC X(08)           VALUE
              'BATCH'.
           05 WRK-MASCARA              PIC ZZ.ZZZ.ZZZ.ZZ9 VALUE ZEROS.
           05 WRK-DATA.
             10 WRK-AAAA               PIC 9(04)           VALUE ZEROS.
             10 WRK-MM                 PIC 9(02)           VALUE ZEROS.
             10 WRK-DD                 PIC 9(02)           VALUE ZEROS.
           05 WRK-DOCOR-EVNTO          REDEFINES WRK-DATA  PIC 9(08).
           05 WRK-DATA-AUX-X.
             10 WRK-AAAA-AUX           PIC 9(04)           VALUE ZEROS.
             10 WRK-MM-AUX             PIC 9(02)           VALUE ZEROS.
             10 WRK-DD-AUX             PIC 9(02)           VALUE ZEROS.
           05 WRK-DATA-AUX         REDEFINES WRK-DATA-AUX-X  PIC 9(08).
           05 WRK-DATA-DB2.
             10 WRK-DD-DB2             PIC 9(02)           VALUE ZEROS.
             10 FILLER                 PIC X(01)           VALUE '.'.
             10 WRK-MM-DB2             PIC 9(02)           VALUE ZEROS.
             10 FILLER                 PIC X(01)           VALUE '.'.
             10 WRK-AAAA-DB2           PIC 9(04)           VALUE ZEROS.
032011     05 WRK-QTDE-OBTIDA          PIC 9(05) COMP-3    VALUE ZEROS.
032011     05 WRK-QTDE-OBTIDA-AUX      PIC S9(05)          VALUE ZEROS.
032011     05 WRK-QTDE-OBTIDA-R  REDEFINES   WRK-QTDE-OBTIDA-AUX
032011                                 PIC 9(05).
           05  WRK-AUX-5-S             PIC +9(5)           VALUE ZEROS.
           05  FILLER                  REDEFINES           WRK-AUX-5-S.
               10 FILLER               PIC  X(1).
               10 FILLER               PIC  X(2).
               10 WRK-AUX-5-3          PIC  9(3).

032011     05 WRK-QTDE-OBT-MV          PIC 9(05) COMP-3    VALUE ZEROS.
032011     05 WRK-QTDE-OBT-MV-AUX      PIC S9(05)          VALUE ZEROS.
032011     05 WRK-QTDE-OBT-MV-R  REDEFINES   WRK-QTDE-OBT-MV-AUX
032011                                 PIC 9(05).

032011     05 WRK-QTDE-UTIL-TARIFA     PIC 9(05) COMP-3    VALUE ZEROS.
032011     05 WRK-QTDE-UTIL-TARIFA-AUX PIC S9(05)          VALUE ZEROS.
032011     05 WRK-QTDE-UTIL-TARIFAR
032011                      REDEFINES  WRK-QTDE-UTIL-TARIFA-AUX
032011                                 PIC 9(05).

           05 WRK-QTDE-AUX             PIC 9(09)           VALUE ZEROS.
           05 WRK-QTDE-AUXR  REDEFINES  WRK-QTDE-AUX.
              07  FILLER               PIC 9(04).
              07  WRK-QTDE-AUX-5       PIC 9(05).
           05 WRK-QTDE-EXC-DIA         PIC 9(05) COMP-3    VALUE ZEROS.

           05 WRK-CTPO-COMPO-PCOTE     PIC S9(001) COMP-3  VALUE ZEROS.

           05 WRK-CCOMPO-PCOTE         PIC S9(005) COMP-3  VALUE ZEROS.

           05 WRK-QSERVC-UTLZD-PCOTE   PIC S9(007) COMP-3  VALUE ZEROS.

           05 WRK-CKRS0100             PIC X(08)     VALUE 'CKRS0100'.

           05  WRK-DATA-X10            PIC X(10)    VALUE SPACES.
           05  WRK-DATA-X10R    REDEFINES  WRK-DATA-X10.
               07  FILLER              PIC X(03).
               07  WRK-MES-X10         PIC 9(02).
               07  FILLER              PIC X(01).
               07  WRK-ANO-X10         PIC 9(04).

151009     05 WRK-EVENTO-ISENTO        PIC X(01)     VALUE 'N'.

           05 WRK-LER-EVCADSAI         PIC X(01)     VALUE 'N'.
           05 WRK-LER-COMPCLII         PIC X(01)     VALUE 'N'.
           05 WRK-RESTO-C              PIC 9(003) VALUE ZEROS COMP-3.
           05 WRK-INTEIRO-C            PIC 9(004) VALUE ZEROS COMP-3.
           05 WRK-AAAA-DB2-C           PIC 9(004) VALUE ZEROS COMP-3.
           05 WRK-CAMPOS-SIGNAL.
              07 WRK-AGENCIA-S         PIC S9(05)  VALUE ZEROS.
              07 WRK-AGENCIA-R REDEFINES WRK-AGENCIA-S
                                       PIC  9(05).
              07 WRK-CONTA-S           PIC S9(13)  VALUE ZEROS.
              07 WRK-CONTA-R REDEFINES WRK-CONTA-S
                                       PIC  9(13).
              07 WRK-COMPONENTE-S      PIC S9(05)  VALUE ZEROS.
              07 WRK-COMPONENTE-R REDEFINES WRK-COMPONENTE-S
                                       PIC  9(05).
2603       05  WRK-VLR-BRUTO           PIC S9(009)V9(2) COMP-3
2603                                                       VALUE ZEROS.

       01 WRK-AREA-RESTART.
           10  WRK-LIDOS-COMPCLII  PIC 9(11)           VALUE ZEROS.
           10  WRK-LIDOS-EVCADSAI  PIC 9(11)           VALUE ZEROS.
           10  WRK-GRAVA-EXCED92O  PIC 9(11)           VALUE ZEROS.
           10  WRK-GRAVA-ISENT41O  PIC 9(11)           VALUE ZEROS.
310709     10  WRK-GRAVA-ST18T41O  PIC 9(11)           VALUE ZEROS.
           10  WRK-GRAVA-ISENT92O  PIC 9(11)           VALUE ZEROS.
           10  WRK-GRAVA-EXCEDRES  PIC 9(11)           VALUE ZEROS.
           10  WRK-GRAVA-STGRPSRV  PIC 9(11)           VALUE ZEROS.
310709     10  WRK-GRAVA-ST18T92O  PIC 9(11)           VALUE ZEROS.
           10  WRK-GRAVA-EVNBATEO  PIC 9(11)           VALUE ZEROS.

       01  WRK-9-9                      PIC  9(09)       VALUE ZEROS.
       01  FILLER                       REDEFINES  WRK-9-9.
           03 FILLER                    PIC  X(02).
           03 WRK-9-7                   PIC  9(07).

       01  WRK-S9-7                     PIC S9(07)       VALUE ZEROS.
       01  FILLER                       REDEFINES  WRK-S9-7.
           03 WRK-9-7-SEM-SINAL         PIC  9(07).

       01  WRK-S9-7-SCOMP               PIC S9(07)       VALUE ZEROS.

RU0125 01 WRK-CPTCAO-GFCTB041          PIC  X(010) VALUE SPACES.
RU0125 01 FILLER                       REDEFINES WRK-CPTCAO-GFCTB041.
           05 WRK-CPTCAO-B041          PIC 9(002).
RU0125     05 FILLER                   PIC X(008).
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      MENSAGEM DE ERRO        *'.
      *----------------------------------------------------------------*

       01  WRK-MSG-FS-ERRO.
           05  FILLER                  PIC X(07)           VALUE SPACES.
           05  FILLER                  PIC X(02)           VALUE '* '.
           05  FILLER                  PIC X(05)           VALUE
               'ERRO '.
           05  WRK-OPERACAO            PIC X(13)           VALUE SPACES.
           05  FILLER                  PIC X(12)           VALUE
               ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC X(08)           VALUE SPACES.
           05  FILLER                  PIC X(17)           VALUE
               ' - FILE-STATUS = '.
           05  WRK-FILE-STATUS         PIC X(02)           VALUE SPACES.
           05  FILLER                  PIC X(02)           VALUE ' *'.
           05  FILLER                  PIC X(07)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       AREA DE EVENTOS        *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTVA'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       AREA DE COMPONENTES    *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTVB'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       AREA DE HISTORICO      *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTV4'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       AREA DE RESTART        *'.
      *----------------------------------------------------------------*

       COPY 'I#CKRS01'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       AREA DA POOL7100       *'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       AREA PARA DB2          *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB034
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    FIM DA WORKING GFCT1838   *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    CONTROLE GERAL DE PROCESSOS                                 *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*
CAPG=I     CALL 'CKRS1000'.

           PERFORM 0100-INICIALIZAR.

           PERFORM 1100-VERIFICAR-VAZIO.

           PERFORM 1400-ACESSAR-GFCTB0A1.

           PERFORM 1500-PROCESSAR-DADOS
             UNTIL WRK-CHAVE-EVENTO    EQUAL HIGH-VALUES.

           PERFORM 9000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ABRIR ARQUIVOS                                              *
      *----------------------------------------------------------------*
       0100-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 0200-START-RESTART.

           OPEN INPUT  COMPCLII
                       EVCADSAI
                OUTPUT EXCED92O
                       ISENT41O
310709                 ST18T41O
                       ISENT92O
                       EXCEDRES
                       STGRPSRV
310709                 ST18T92O
                       EVNBATEO.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAR CONEXAO COM DB2                          *
      *----------------------------------------------------------------*
       0200-START-RESTART              SECTION.
      *----------------------------------------------------------------*

           INITIALIZE CKRS01-INTERFACE.

           MOVE  SPACES                      TO CK01-PLAN.
           MOVE  'DB2 '                      TO CK01-ID-DB2.
           MOVE  'I'                         TO CK01-FUNCAO.
           MOVE  SPACES                      TO CK01-AREA-RESTART.
           MOVE  ZEROS                       TO CK01-TAM-AREA-RESTART.

           CALL WRK-CKRS0100           USING CKRS01-INTERFACE.

           IF  CK01-CODIGO-RETORNO     NOT EQUAL ZEROS
               DISPLAY '******** NA ABERTURA ********'
               PERFORM 0300-TRATAR-ERRO-RESTART
           END-IF.

           IF  CK01-RESTART
               MOVE CK01-AREA-RESTART(1:CK01-TAM-AREA-RESTART)
                                       TO WRK-AREA-RESTART
               MOVE WRK-LIDOS-COMPCLII TO ACU-LIDOS-COMPCLII
               MOVE WRK-LIDOS-EVCADSAI TO ACU-LIDOS-EVCADSAI
               MOVE WRK-GRAVA-EXCED92O TO ACU-GRAVA-EXCED92O
               MOVE WRK-GRAVA-ISENT41O TO ACU-GRAVA-ISENT41O
310709         MOVE WRK-GRAVA-ST18T41O TO ACU-GRAVA-ST18T41O
               MOVE WRK-GRAVA-ISENT92O TO ACU-GRAVA-ISENT92O
               MOVE WRK-GRAVA-EXCEDRES TO ACU-GRAVA-EXCEDRES
               MOVE WRK-GRAVA-STGRPSRV TO ACU-GRAVA-STGRPSRV
310709         MOVE WRK-GRAVA-ST18T92O TO ACU-GRAVA-ST18T92O
               MOVE WRK-GRAVA-EVNBATEO TO ACU-GRAVA-EVNBATEO
           END-IF.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAR ERRO NO ACESSO AO MODULO CKRS0100        *
      *----------------------------------------------------------------*
       0300-TRATAR-ERRO-RESTART        SECTION.
      *----------------------------------------------------------------*

           MOVE 'APL'                  TO ERR-TIPO-ACESSO.

           IF  CK01-CODIGO-RETORNO     EQUAL 8
               EVALUATE CK01-CODIGO-MENSAGEM
                   WHEN 'CKRS0001'
                        MOVE 'CKRS0100 - CODIGO DE FUNCAO INCORRETO'
                                       TO ERR-TEXTO

                   WHEN 'CKRS0002'
                        MOVE 'CKRS0100 - JA FOI FEITA UMA CHAMADA EM MOD
      -                      'O INITIALIZE'
                                       TO ERR-TEXTO

                   WHEN 'CKRS0003'
                        MOVE 'CKRS0100 - NAO FOI EFETUADA CHAMADA EM MOD
      -                      'O INITIALIZE'
                                       TO ERR-TEXTO

                   WHEN 'CKRS0004'
                        MOVE 'CKRS0100 - CAMPO DB2-ID DEVE ESTAR INFORMA
      -                      'DO'      TO ERR-TEXTO

                   WHEN 'CKRS0005'
                        MOVE 'CKRS0100 - O PROCESSO NAO SE ENCONTRA CADA
      -                      'STRADO'  TO ERR-TEXTO

                   WHEN OTHER
                        MOVE 'CKRS0100 - ERRO NAO IDENTIFICADO'
                                       TO ERR-TEXTO
                        DISPLAY '************** GFCT1838 **************'
                        DISPLAY '*                                    *'
                        DISPLAY '* CK01-CODIGO-RETORNO : '
                                   CK01-CODIGO-RETORNO    '           *'
                        DISPLAY '* CK01-CODIGO-MENSAGEM: '
                                   CK01-CODIGO-MENSAGEM         '     *'
                        DISPLAY '*                                    *'
                        DISPLAY '************** GFCT1838 **************'
               END-EVALUATE
           ELSE
               MOVE 'CKRS0100 - ERRO GRAVE - RETURN <> DE ZEROS E 8'
                                       TO ERR-TEXTO
               DISPLAY '************** GFCT1838 **************'
               DISPLAY '*                                    *'
               DISPLAY '* CK01-CODIGO-RETORNO : '
                          CK01-CODIGO-RETORNO    '           *'
               DISPLAY '* CK01-CODIGO-MENSAGEM: '
                          CK01-CODIGO-MENSAGEM         '     *'
               DISPLAY '*                                    *'
               DISPLAY '************** GFCT1838 **************'
           END-IF.

           PERFORM 9999-TRATAR-ERRO.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA TESTAR FILE STATUS DOS ARQUIVOS                      *
      *----------------------------------------------------------------*
       0400-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 0500-TESTAR-FS-COMPCLII.

           PERFORM 0600-TESTAR-FS-EVCADSAI.

           PERFORM 0700-TESTAR-FS-EXCED92O.

           PERFORM 0750-TESTAR-FS-EXCEDRES.

           PERFORM 0800-TESTAR-FS-ISENT41O.

310709     PERFORM 0820-TESTAR-FS-ST18T41O.

           PERFORM 0850-TESTAR-FS-ISENT92O.

           PERFORM 0950-TESTAR-FS-STGRPSRV.

310709     PERFORM 0970-TESTAR-FS-ST18T92O.

           PERFORM 1000-TESTAR-FS-EVNBATEO.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA TESTAR FILE STATUS DO ARQUIVO COMPCLII               *
      *----------------------------------------------------------------*
       0500-TESTAR-FS-COMPCLII         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-COMPCLII          NOT EQUAL '00'
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE 'COMPCLII'          TO WRK-NOME-ARQ
              MOVE WRK-FS-COMPCLII     TO WRK-FILE-STATUS
              MOVE WRK-MSG-FS-ERRO     TO ERR-TEXTO
              PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA TESTAR FILE STATUS DO ARQUIVO EVCADSAI               *
      *----------------------------------------------------------------*
       0600-TESTAR-FS-EVCADSAI         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-EVCADSAI          NOT EQUAL '00'
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE 'EVCADSAI'          TO WRK-NOME-ARQ
              MOVE WRK-FS-EVCADSAI     TO WRK-FILE-STATUS
              MOVE WRK-MSG-FS-ERRO     TO ERR-TEXTO
              PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA TESTAR FILE STATUS DO ARQUIVO EXCED92O               *
      *----------------------------------------------------------------*
       0700-TESTAR-FS-EXCED92O         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-EXCED92O          NOT EQUAL '00'
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE 'EXCED92O'          TO WRK-NOME-ARQ
              MOVE WRK-FS-EXCED92O     TO WRK-FILE-STATUS
              MOVE WRK-MSG-FS-ERRO     TO ERR-TEXTO
              PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA TESTAR FILE STATUS DO ARQUIVO EXCEDRES               *
      *----------------------------------------------------------------*
       0750-TESTAR-FS-EXCEDRES         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-EXCEDRES          NOT EQUAL '00'
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE 'EXCEDRES'          TO WRK-NOME-ARQ
              MOVE WRK-FS-EXCEDRES     TO WRK-FILE-STATUS
              MOVE WRK-MSG-FS-ERRO     TO ERR-TEXTO
              PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0750-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA TESTAR FILE STATUS DO ARQUIVO ISENT41O               *
      *----------------------------------------------------------------*
       0800-TESTAR-FS-ISENT41O         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-ISENT41O          NOT EQUAL '00'
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE 'ISENT41O'          TO WRK-NOME-ARQ
              MOVE WRK-FS-ISENT41O     TO WRK-FILE-STATUS
              MOVE WRK-MSG-FS-ERRO     TO ERR-TEXTO
              PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

310709*----------------------------------------------------------------*
310709*    ROTINA TESTAR FILE STATUS DO ARQUIVO ST18T41O               *
310709*----------------------------------------------------------------*
310709 0820-TESTAR-FS-ST18T41O         SECTION.
310709*----------------------------------------------------------------*
310709
310709     IF WRK-FS-ST18T41O          NOT EQUAL '00'
310709        MOVE 'APL'               TO ERR-TIPO-ACESSO
310709        MOVE 'ST18T41O'          TO WRK-NOME-ARQ
310709        MOVE WRK-FS-ST18T41O     TO WRK-FILE-STATUS
310709        MOVE WRK-MSG-FS-ERRO     TO ERR-TEXTO
310709        PERFORM 9999-TRATAR-ERRO
310709     END-IF.
310709
310709*----------------------------------------------------------------*
310709 0820-99-FIM.                    EXIT.
310709*----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA TESTAR FILE STATUS DO ARQUIVO ISENT92O               *
      *----------------------------------------------------------------*
       0850-TESTAR-FS-ISENT92O         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-ISENT92O          NOT EQUAL '00'
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE 'ISENT92O'          TO WRK-NOME-ARQ
              MOVE WRK-FS-ISENT92O     TO WRK-FILE-STATUS
              MOVE WRK-MSG-FS-ERRO     TO ERR-TEXTO
              PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0850-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA TESTAR FILE STATUS DO ARQUIVO STGRPSRV               *
      *----------------------------------------------------------------*
       0950-TESTAR-FS-STGRPSRV         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-STGRPSRV          NOT EQUAL '00'
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE 'STGRPSRV'          TO WRK-NOME-ARQ
              MOVE WRK-FS-STGRPSRV     TO WRK-FILE-STATUS
              MOVE WRK-MSG-FS-ERRO     TO ERR-TEXTO
              PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0950-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

310709*----------------------------------------------------------------*
310709*    ROTINA TESTAR FILE STATUS DO ARQUIVO ST18T92O               *
310709*----------------------------------------------------------------*
310709 0970-TESTAR-FS-ST18T92O         SECTION.
310709*----------------------------------------------------------------*
310709
310709     IF WRK-FS-ST18T92O          NOT EQUAL '00'
310709        MOVE 'APL'               TO ERR-TIPO-ACESSO
310709        MOVE 'ST18T92O'          TO WRK-NOME-ARQ
310709        MOVE WRK-FS-ST18T92O     TO WRK-FILE-STATUS
310709        MOVE WRK-MSG-FS-ERRO     TO ERR-TEXTO
310709        PERFORM 9999-TRATAR-ERRO
310709     END-IF.
310709
310709*----------------------------------------------------------------*
310709 0970-99-FIM.                    EXIT.
310709*----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA TESTAR FILE STATUS DO ARQUIVO EVNBATEO               *
      *----------------------------------------------------------------*
       1000-TESTAR-FS-EVNBATEO         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-EVNBATEO          NOT EQUAL '00'
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE 'EVNBATEO'          TO WRK-NOME-ARQ
              MOVE WRK-FS-EVNBATEO     TO WRK-FILE-STATUS
              MOVE WRK-MSG-FS-ERRO     TO ERR-TEXTO
              PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA VERIFICAR ARQUIVOS VAZIOS                            *
      *----------------------------------------------------------------*
       1100-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 1200-LER-COMPCLII.

           IF  WRK-CHAVE-COMPONENTE    EQUAL HIGH-VALUES
               DISPLAY '*************** GFCT1838 ***************'
               DISPLAY '*                                      *'
               DISPLAY '*   NAO EXISTEM COMPONENTES            *'
               DISPLAY '*                                      *'
               DISPLAY '*       TERMINO NORMAL                 *'
               DISPLAY '*                                      *'
               DISPLAY '*************** GFCT1838 ***************'
           END-IF.

           PERFORM 1300-LER-EVCADSAI.

           IF  WRK-CHAVE-EVENTO        EQUAL HIGH-VALUES
               DISPLAY '*************** GFCT1838 ***************'
               DISPLAY '*                                      *'
               DISPLAY '*   NAO EXISTEM EVENTOS CONTABEIS      *'
               DISPLAY '*                                      *'
               DISPLAY '*      PROCESSO TERMINA NORMAL         *'
               DISPLAY '*                                      *'
               DISPLAY '*************** GFCT1838 ***************'
           ELSE
               MOVE WRK-CHAVE-EVENTO   TO WRK-CHAVE-W-EVENTO
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA LER ARQUIVO COMPCLII                                 *
      *----------------------------------------------------------------*
       1200-LER-COMPCLII               SECTION.
      *----------------------------------------------------------------*

           READ COMPCLII               INTO GFCTVB-COMPONENTES.

           IF WRK-FS-COMPCLII          EQUAL '10'
              MOVE HIGH-VALUES         TO WRK-CHAVE-COMPONENTE
              GO                       TO 1200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0500-TESTAR-FS-COMPCLII.

           ADD  1                      TO ACU-LIDOS-COMPCLII.

           MOVE GFCTVB-AGENCIA         TO WRK-COMPO-AGENCIA.
           MOVE GFCTVB-CONTA           TO WRK-COMPO-CONTA.
           MOVE GFCTVB-COMPO-TARIFA    TO WRK-COMPO-COMPONENTE.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA LER ARQUIVO EVCADSAI                                 *
      *----------------------------------------------------------------*
       1300-LER-EVCADSAI               SECTION.
      *----------------------------------------------------------------*

           READ EVCADSAI               INTO GFCTVA-GFCTB092.

           IF WRK-FS-EVCADSAI          EQUAL '10'
              MOVE HIGH-VALUES         TO WRK-CHAVE-EVENTO
              GO                       TO 1300-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0600-TESTAR-FS-EVCADSAI.

           ADD  1                      TO ACU-LIDOS-EVCADSAI.

           MOVE GFCTVA-CAG-DSTNO-MOVTO TO WRK-AGENCIA-S.
           MOVE WRK-AGENCIA-R          TO WRK-EVENTO-AGENCIA.
           MOVE GFCTVA-CCTA-DSTNO-MOVTO
                                       TO WRK-CONTA-S.
           MOVE WRK-CONTA-R            TO WRK-EVENTO-CONTA.
           MOVE GFCTVA-CSERVC-TARIF    TO WRK-COMPONENTE-S.
           MOVE WRK-COMPONENTE-R       TO WRK-EVENTO-COMPONENTE.
           MOVE GFCTVA-DOCOR-EVNTO     TO WRK-DATA-DB2.
           MOVE WRK-AAAA-DB2           TO WRK-AAAA-AUX.
           MOVE WRK-MM-DB2             TO WRK-MM-AUX.
           MOVE WRK-DD-DB2             TO WRK-DD-AUX.
           MOVE WRK-DATA-AUX           TO WRK-DOCOR-EVNTO.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA DE ACESSO A TABELA GFCTB0A1
      *----------------------------------------------------------------*
       1400-ACESSAR-GFCTB0A1           SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
               SELECT  DPROCM_ATUAL
               INTO   :GFCTB0A1.DPROCM-ATUAL
               FROM    DB2PRD.PARM_DATA_PROCM
               WHERE   CSIST_PRINC     = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM'  TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0010'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     TRATAR BALANCE LINE COM CHAVES EVENTO X COMPONENTES        *
      *----------------------------------------------------------------*
       1500-PROCESSAR-DADOS            SECTION.
      *----------------------------------------------------------------*

           MOVE  'N'                   TO  WRK-LER-EVCADSAI
                                           WRK-LER-COMPCLII.

           IF  WRK-CHAVE-EVENTO        EQUAL WRK-CHAVE-COMPONENTE
               IF  WRK-DOCOR-EVNTO     NOT LESS GFCTVB-DINIC-PERIODO AND
                   WRK-DOCOR-EVNTO     NOT GREATER GFCTVB-DFIM-PERIODO
                   PERFORM 1600-VER-SE-JATEM-EVENTOS
                   MOVE  'S'           TO  WRK-LER-EVCADSAI
               ELSE
                   IF  WRK-DOCOR-EVNTO LESS GFCTVB-DINIC-PERIODO
                       PERFORM 2900-GRAVAR-EVNBATEO
                       MOVE  'S'       TO  WRK-LER-EVCADSAI
                   ELSE
                       MOVE  'S'       TO  WRK-LER-COMPCLII
                   END-IF
               END-IF
           ELSE
               IF  WRK-CHAVE-EVENTO    LESS WRK-CHAVE-COMPONENTE
                   PERFORM 2900-GRAVAR-EVNBATEO
                   MOVE  'S'           TO  WRK-LER-EVCADSAI
               ELSE
                   MOVE  'S'           TO  WRK-LER-COMPCLII
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      *    TRATAMENTO PARA IDENTIFICAR QUAL ARQUIVO DEVERA SER LIDO    *
      *       DEVIDO A NECESSIDADE DE SER UTILIZADO A ROTINA DE        *
      *       CHECKPOINT/RESTART SEMPRE ANTES DA LEITURA DOS ARQUIVOS  *
      *----------------------------------------------------------------*

           PERFORM 2800-CHECK-POINT.

           IF  WRK-LER-EVCADSAI   EQUAL  'S'
               PERFORM 1300-LER-EVCADSAI
           END-IF.

           IF  WRK-LER-COMPCLII   EQUAL  'S'
               PERFORM 1200-LER-COMPCLII
           END-IF.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ROTINA PARA VERIFICAR SE JA TEM EVENTOS                    *
      *----------------------------------------------------------------*
       1600-VER-SE-JATEM-EVENTOS       SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVA-CBCO-DSTNO-MOVTO
                                       TO CBCO-DSTNO-MOVTO OF GFCTB034.
           MOVE GFCTVA-CAG-DSTNO-MOVTO TO CAG-DSTNO-MOVTO  OF GFCTB034.
           MOVE GFCTVA-CCTA-DSTNO-MOVTO
                                       TO CCTA-DSTNO-MOVTO OF GFCTB034.

VINI       IF  GFCTVB-GRUPO        EQUAL ZEROS
VINI           MOVE 1                  TO CTPO-COMPO-PCOTE OF GFCTB034
0209           MOVE GFCTVB-COMPO-TARIFA
0209                                   TO CCOMPO-PCOTE     OF GFCTB034
VINI       ELSE
VINI           MOVE 2                  TO CTPO-COMPO-PCOTE OF GFCTB034
0209           MOVE GFCTVB-GRUPO
0209                                   TO CCOMPO-PCOTE     OF GFCTB034
VINI       END-IF.

           MOVE '  .  .    '               TO WRK-DATA-DB2.
           MOVE 1                          TO WRK-DD-DB2.
           MOVE GFCTVA-DOCOR-EVNTO         TO WRK-DATA-X10.
           MOVE WRK-MES-X10                TO WRK-MM-DB2.
           MOVE WRK-ANO-X10                TO WRK-AAAA-DB2.
           MOVE WRK-DATA-DB2           TO DINIC-CTRL-PCOTE OF GFCTB034.

           PERFORM 1700-ACESSAR-GFCTB034.

      *----------------------------------------------------------------*
       1600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ROTINA PARA ACESSAR GFCTB034                               *
      *----------------------------------------------------------------*
       1700-ACESSAR-GFCTB034           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                   CBCO_DSTNO_MOVTO,
                   CAG_DSTNO_MOVTO,
                   CCTA_DSTNO_MOVTO,
                   CTPO_COMPO_PCOTE,
                   CCOMPO_PCOTE,
                   DINIC_CTRL_PCOTE,
                   CTARIF_PCOTE,
                   QSERVC_UTLZD_PCOTE,
                   DFIM_CTRL_PCOTE,
151009             QMOVTO_COMPO_PCOTE
             INTO  :GFCTB034.CBCO-DSTNO-MOVTO,
                   :GFCTB034.CAG-DSTNO-MOVTO,
                   :GFCTB034.CCTA-DSTNO-MOVTO,
                   :GFCTB034.CTPO-COMPO-PCOTE,
                   :GFCTB034.CCOMPO-PCOTE,
                   :GFCTB034.DINIC-CTRL-PCOTE,
                   :GFCTB034.CTARIF-PCOTE,
                   :GFCTB034.QSERVC-UTLZD-PCOTE,
                   :GFCTB034.DFIM-CTRL-PCOTE,
151009             :GFCTB034.QMOVTO-COMPO-PCOTE
             FROM   DB2PRD.CTRL_PCOTE_MOVTO
             WHERE CBCO_DSTNO_MOVTO    = :GFCTB034.CBCO-DSTNO-MOVTO
               AND CAG_DSTNO_MOVTO     = :GFCTB034.CAG-DSTNO-MOVTO
               AND CCTA_DSTNO_MOVTO    = :GFCTB034.CCTA-DSTNO-MOVTO
               AND CTPO_COMPO_PCOTE    = :GFCTB034.CTPO-COMPO-PCOTE
               AND CCOMPO_PCOTE        = :GFCTB034.CCOMPO-PCOTE
               AND DINIC_CTRL_PCOTE    = :GFCTB034.DINIC-CTRL-PCOTE
           END-EXEC.

           IF  (SQLCODE         NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0        EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'CTRL_PCOTE_MOVTO'
                                       TO ERR-DBD-TAB
                MOVE 'SELECT    '      TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0020'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-TRATAR-ERRO
           END-IF.

VINI       IF  GFCTVB-GRUPO        EQUAL ZEROS
VINI           PERFORM  1750-TRATAR-COMPOTAR
VINI       ELSE
VINI           PERFORM  1760-TRATAR-COMPOGRU
VINI       END-IF.

      *----------------------------------------------------------------*
       1700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ROTINA PARA TRATAR COMPONENTE TARIFA                       *
      *----------------------------------------------------------------*
       1750-TRATAR-COMPOTAR            SECTION.
      *----------------------------------------------------------------*

           IF  SQLCODE                 EQUAL ZEROS
               COMPUTE WRK-QTDE-OBTIDA = QSERVC-UTLZD-PCOTE OF GFCTB034
                                       + 1
               MOVE WRK-QTDE-OBTIDA    TO WRK-QTDE-OBTIDA-AUX
151009         PERFORM 1753-TRAT-EXISTE-B034
           ELSE
151009         PERFORM 1758-TRAT-N-EXISTE-B034
           END-IF.

      *----------------------------------------------------------------*
       1750-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

151009*----------------------------------------------------------------*
151009*     IDENTIFICA SE O COMPONENTE TRATA POR QTDE DE MOVIMENTO     *
151009*       QUANDO DA JA EXISTENCIA DE UMA UTILIZACAO                *
151009*----------------------------------------------------------------*
151009 1753-TRAT-EXISTE-B034          SECTION.
151009*----------------------------------------------------------------*
151009
151009     IF  GFCTVB-CIDTFD-CTRL-UTILZ  EQUAL  1
151009
151009       IF  WRK-CHAVE-EVENTO    NOT EQUAL  WRK-CHAVE-EVENTO-ANT
151009
151009         MOVE WRK-CHAVE-EVENTO   TO WRK-CHAVE-EVENTO-ANT
151009         MOVE 'N'                TO WRK-EVENTO-ISENTO
151009         COMPUTE WRK-QTDE-OBT-MV = QMOVTO-COMPO-PCOTE OF GFCTB034
151009                                 + 1
151009         MOVE WRK-QTDE-OBT-MV    TO WRK-QTDE-OBT-MV-AUX
151009
151009         IF  WRK-QTDE-OBT-MV-R   GREATER GFCTVB-FRANQUIAS
151009             PERFORM 1900-TRATAR-EXCEDENTE
151009             ADD  1  TO  QMOVTO-COMPO-PCOTE OF GFCTB034
151009             ADD  1  TO  QSERVC-UTLZD-PCOTE OF GFCTB034
151009             PERFORM 2150-ATUAL-MV-GFCTB034
151009         ELSE
151009             MOVE 'S'            TO WRK-EVENTO-ISENTO
151009             PERFORM 2200-TRATAR-ISENTO
151009             ADD  1  TO  QMOVTO-COMPO-PCOTE OF GFCTB034
151009             ADD  1  TO  QSERVC-UTLZD-PCOTE OF GFCTB034
151009             PERFORM 2150-ATUAL-MV-GFCTB034
151009         END-IF
151009
151009       ELSE
151009
151009         IF  WRK-EVENTO-ISENTO   EQUAL 'S'
151009             PERFORM 2200-TRATAR-ISENTO
151009             ADD  1  TO  QSERVC-UTLZD-PCOTE OF GFCTB034
151009             PERFORM 2100-ATUALIZAR-GFCTB034
151009         ELSE
151009             PERFORM 1900-TRATAR-EXCEDENTE
151009             ADD  1  TO  QSERVC-UTLZD-PCOTE OF GFCTB034
151009             PERFORM 2100-ATUALIZAR-GFCTB034
151009         END-IF
151009
151009       END-IF
151009
151009     ELSE
151009
151009       IF  WRK-QTDE-OBTIDA-R   GREATER GFCTVB-FRANQUIAS
151009           PERFORM 1900-TRATAR-EXCEDENTE
151009           ADD  1  TO  QSERVC-UTLZD-PCOTE OF GFCTB034
151009           PERFORM 2100-ATUALIZAR-GFCTB034
151009       ELSE
151009           PERFORM 2200-TRATAR-ISENTO
151009           ADD  1  TO  QSERVC-UTLZD-PCOTE OF GFCTB034
151009           PERFORM 2100-ATUALIZAR-GFCTB034
151009       END-IF
151009
151009     END-IF.
151009
151009*----------------------------------------------------------------*
151009 1753-99-FIM.                    EXIT.
151009*----------------------------------------------------------------*

151009*----------------------------------------------------------------*
151009*     IDENTIFICA SE O COMPONENTE TRATA POR QTDE DE MOVIMENTO     *
151009*       QUANDO DA   NAO   EXISTENCIA DE UMA UTILIZACAO           *
151009*----------------------------------------------------------------*
151009 1758-TRAT-N-EXISTE-B034        SECTION.
151009*----------------------------------------------------------------*
151009
151009     IF  GFCTVB-CIDTFD-CTRL-UTILZ  EQUAL  1
151009
151009       MOVE WRK-CHAVE-EVENTO     TO WRK-CHAVE-EVENTO-ANT
151009
151009       IF  GFCTVB-FRANQUIAS      EQUAL  ZEROS
151009         MOVE 'N'                TO WRK-EVENTO-ISENTO
151009         PERFORM 1900-TRATAR-EXCEDENTE
151009         PERFORM 2620-INSERIR-MV-GFCTB034
151009       ELSE
151009         MOVE 'S'            TO WRK-EVENTO-ISENTO
151009         PERFORM 2200-TRATAR-ISENTO
151009         PERFORM 2620-INSERIR-MV-GFCTB034
151009       END-IF
151009
151009     ELSE
151009
151009       IF  GFCTVB-FRANQUIAS    EQUAL ZEROS
151009           PERFORM 1900-TRATAR-EXCEDENTE
151009           PERFORM 2600-INSERIR-GFCTB034
151009       ELSE
151009           PERFORM 2200-TRATAR-ISENTO
151009           PERFORM 2600-INSERIR-GFCTB034
151009       END-IF
151009
151009     END-IF.
151009
151009*----------------------------------------------------------------*
151009 1758-99-FIM.                    EXIT.
151009*----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ROTINA PARA TRATAR COMPONENTE GRUPO DE SERVICO             *
      *----------------------------------------------------------------*
       1760-TRATAR-COMPOGRU            SECTION.
      *----------------------------------------------------------------*

           IF  SQLCODE                 EQUAL ZEROS
               IF  GFCTVB-QREST-UTIL-TARIF  NOT EQUAL  ZEROS
                   PERFORM 1761-OBTER-QTDE-UTILIZ-TARIFA

                   COMPUTE WRK-QTDE-OBTIDA
                                        =  WRK-QSERVC-UTLZD-PCOTE + 1

                   MOVE WRK-QTDE-OBTIDA TO WRK-QTDE-OBTIDA-AUX

032011             MOVE GFCTVB-QREST-UTIL-TARIF
032011                                  TO WRK-QTDE-UTIL-TARIFA
032011                                     WRK-QTDE-UTIL-TARIFA-AUX
                   IF  WRK-QTDE-OBTIDA-AUX  GREATER
032011                                      WRK-QTDE-UTIL-TARIFA-AUX
                       PERFORM 3800-TRATAR-EXCED-RESTR

                   ELSE
310709                 IF GFCTVB-CPER-COBR-COMPO EQUAL 1
310709                     PERFORM 4200-TRATAR-ISENTO18
310709                     ADD 1 TO QSERVC-UTLZD-PCOTE OF GFCTB034
310709                     PERFORM 2100-ATUALIZAR-GFCTB034
310709                 ELSE
                           COMPUTE WRK-QTDE-OBTIDA =
                                     QSERVC-UTLZD-PCOTE OF GFCTB034 + 1

                           MOVE WRK-QTDE-OBTIDA
                                        TO WRK-QTDE-OBTIDA-AUX
                           MOVE WRK-QTDE-OBTIDA-AUX TO WRK-AUX-5-S
                           IF  WRK-AUX-5-3
                                        GREATER GFCTVB-FRANQUIAS
                               PERFORM 4100-GRAVAR-STGRPSRV
                           ELSE
                               PERFORM 2200-TRATAR-ISENTO

                               ADD  1      TO QSERVC-UTLZD-PCOTE
                                                            OF GFCTB034

                               PERFORM 2100-ATUALIZAR-GFCTB034

                               PERFORM 1800-TRATAR-COMPO-PACOTE-TIP-4
                           END-IF
310709                 END-IF
                   END-IF
               ELSE
310709             IF GFCTVB-CPER-COBR-COMPO EQUAL 1
310709                 PERFORM 4200-TRATAR-ISENTO18
310709                 ADD 1 TO QSERVC-UTLZD-PCOTE OF GFCTB034
310709                 PERFORM 2100-ATUALIZAR-GFCTB034
310709             ELSE
                       PERFORM 4100-GRAVAR-STGRPSRV
310709             END-IF
               END-IF
           ELSE
               IF  GFCTVB-QREST-UTIL-TARIF
                                       NOT EQUAL ZEROS
310709             IF GFCTVB-CPER-COBR-COMPO EQUAL 1
310709                 PERFORM 4200-TRATAR-ISENTO18
310709             ELSE
                       PERFORM 2200-TRATAR-ISENTO
310709             END-IF

                   PERFORM 2600-INSERIR-GFCTB034

                   PERFORM 2700-INSERIR-GFCTB034-TIPO-4
               ELSE
310709             IF GFCTVB-CPER-COBR-COMPO EQUAL 1
310709                 PERFORM 4200-TRATAR-ISENTO18
310709             ELSE
                       PERFORM 4100-GRAVAR-STGRPSRV
310709             END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1760-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ROTINA PARA RECUPERAR O TIPO DE COMPONENTE PACOTE 4        *
      *----------------------------------------------------------------*
       1761-OBTER-QTDE-UTILIZ-TARIFA   SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVA-CBCO-DSTNO-MOVTO
                                       TO CBCO-DSTNO-MOVTO OF GFCTB034.
           MOVE GFCTVA-CAG-DSTNO-MOVTO TO CAG-DSTNO-MOVTO  OF GFCTB034.
           MOVE GFCTVA-CCTA-DSTNO-MOVTO
                                       TO CCTA-DSTNO-MOVTO OF GFCTB034.
           MOVE 4                      TO WRK-CTPO-COMPO-PCOTE.
           MOVE GFCTVB-COMPO-TARIFA    TO WRK-CCOMPO-PCOTE.

           MOVE '  .  .    '           TO WRK-DATA-DB2.
           MOVE 1                      TO WRK-DD-DB2.
           MOVE GFCTVA-DOCOR-EVNTO     TO WRK-DATA-X10.
           MOVE WRK-MES-X10            TO WRK-MM-DB2.
           MOVE WRK-ANO-X10            TO WRK-AAAA-DB2.
           MOVE WRK-DATA-DB2           TO DINIC-CTRL-PCOTE OF GFCTB034.

           EXEC SQL
             SELECT
                   QSERVC_UTLZD_PCOTE
             INTO
                   :WRK-QSERVC-UTLZD-PCOTE
             FROM
                    DB2PRD.CTRL_PCOTE_MOVTO
             WHERE
                   CBCO_DSTNO_MOVTO    = :GFCTB034.CBCO-DSTNO-MOVTO
               AND CAG_DSTNO_MOVTO     = :GFCTB034.CAG-DSTNO-MOVTO
               AND CCTA_DSTNO_MOVTO    = :GFCTB034.CCTA-DSTNO-MOVTO
               AND CTPO_COMPO_PCOTE    = :WRK-CTPO-COMPO-PCOTE
               AND CCOMPO_PCOTE        = :WRK-CCOMPO-PCOTE
               AND DINIC_CTRL_PCOTE    = :GFCTB034.DINIC-CTRL-PCOTE
           END-EXEC.

           IF  (SQLCODE         NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0        EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'CTRL_PCOTE_MOVTO'
                                       TO ERR-DBD-TAB
                MOVE 'SELECT    '      TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0030'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-TRATAR-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE ZEROS              TO SQLCODE
                                          WRK-QSERVC-UTLZD-PCOTE
           END-IF.

      *----------------------------------------------------------------*
       1761-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ROTINA PARA TRATAMENTO DO TIPO DE COMPONENTE PACOTE 4      *
      *----------------------------------------------------------------*
       1800-TRATAR-COMPO-PACOTE-TIP-4  SECTION.
      *----------------------------------------------------------------*

           PERFORM 1810-OBTER-COMPO-PACOTE-TIPO-4.

           IF  SQLCODE                 EQUAL ZEROS
               ADD  1                  TO QSERVC-UTLZD-PCOTE
                                                            OF GFCTB034

               PERFORM 2100-ATUALIZAR-GFCTB034
           ELSE
               PERFORM 2700-INSERIR-GFCTB034-TIPO-4
           END-IF.

      *----------------------------------------------------------------*
       1800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ROTINA PARA RECUPERAR O TIPO DE COMPONENTE PACOTE 4        *
      *----------------------------------------------------------------*
       1810-OBTER-COMPO-PACOTE-TIPO-4  SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVA-CBCO-DSTNO-MOVTO
                                       TO CBCO-DSTNO-MOVTO OF GFCTB034.
           MOVE GFCTVA-CAG-DSTNO-MOVTO TO CAG-DSTNO-MOVTO  OF GFCTB034.
           MOVE GFCTVA-CCTA-DSTNO-MOVTO
                                       TO CCTA-DSTNO-MOVTO OF GFCTB034.
           MOVE 4                      TO CTPO-COMPO-PCOTE OF GFCTB034.
           MOVE GFCTVB-COMPO-TARIFA    TO CCOMPO-PCOTE     OF GFCTB034.

           MOVE '  .  .    '           TO WRK-DATA-DB2.
           MOVE 1                      TO WRK-DD-DB2.
           MOVE GFCTVA-DOCOR-EVNTO     TO WRK-DATA-X10.
           MOVE WRK-MES-X10            TO WRK-MM-DB2.
           MOVE WRK-ANO-X10            TO WRK-AAAA-DB2.
           MOVE WRK-DATA-DB2           TO DINIC-CTRL-PCOTE OF GFCTB034.

           EXEC SQL
             SELECT
                   CBCO_DSTNO_MOVTO,
                   CAG_DSTNO_MOVTO,
                   CCTA_DSTNO_MOVTO,
                   CTPO_COMPO_PCOTE,
                   CCOMPO_PCOTE,
                   DINIC_CTRL_PCOTE,
                   CTARIF_PCOTE,
                   QSERVC_UTLZD_PCOTE,
                   DFIM_CTRL_PCOTE
             INTO  :GFCTB034.CBCO-DSTNO-MOVTO,
                   :GFCTB034.CAG-DSTNO-MOVTO,
                   :GFCTB034.CCTA-DSTNO-MOVTO,
                   :GFCTB034.CTPO-COMPO-PCOTE,
                   :GFCTB034.CCOMPO-PCOTE,
                   :GFCTB034.DINIC-CTRL-PCOTE,
                   :GFCTB034.CTARIF-PCOTE,
                   :GFCTB034.QSERVC-UTLZD-PCOTE,
                   :GFCTB034.DFIM-CTRL-PCOTE
             FROM   DB2PRD.CTRL_PCOTE_MOVTO
             WHERE CBCO_DSTNO_MOVTO    = :GFCTB034.CBCO-DSTNO-MOVTO
               AND CAG_DSTNO_MOVTO     = :GFCTB034.CAG-DSTNO-MOVTO
               AND CCTA_DSTNO_MOVTO    = :GFCTB034.CCTA-DSTNO-MOVTO
               AND CTPO_COMPO_PCOTE    = :GFCTB034.CTPO-COMPO-PCOTE
               AND CCOMPO_PCOTE        = :GFCTB034.CCOMPO-PCOTE
               AND DINIC_CTRL_PCOTE    = :GFCTB034.DINIC-CTRL-PCOTE
           END-EXEC.

           IF  (SQLCODE         NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0        EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'CTRL_PCOTE_MOVTO'
                                       TO ERR-DBD-TAB
                MOVE 'SELECT    '      TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0040'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1810-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ROTINA PARA TRATAR EXCEDENTE                               *
      *----------------------------------------------------------------*
       1900-TRATAR-EXCEDENTE           SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHAVE-EVENTO        NOT EQUAL WRK-CHAVE-W-EVENTO
               MOVE QSERVC-UTLZD-PCOTE OF GFCTB034 TO WRK-S9-7-SCOMP
               MOVE WRK-S9-7-SCOMP                 TO WRK-S9-7
               MOVE WRK-9-7-SEM-SINAL              TO WRK-9-7
               MOVE WRK-9-9                        TO WRK-QTDE-AUX
               MOVE 1                  TO WRK-QTDE-EXC-DIA
           ELSE
               ADD 1                   TO WRK-QTDE-EXC-DIA
           END-IF.

           MOVE WRK-QTDE-EXC-DIA        TO GFCTVA-QTD-EXC-DIA.
           MOVE WRK-QTDE-AUX-5          TO GFCTVA-QTD-EXC-ACU.
           MOVE WRK-QTDE-OBTIDA         TO GFCTVA-QTD-FQ-COMPON.
           MOVE GFCTVB-CPER-COBR-COMPO  TO GFCTVA-CPER-COBR-COMPO.
           MOVE GFCTVB-VEXCED-CP-PCT    TO GFCTVA-VEXCED-CP-PCT.
           MOVE GFCTVB-PEXCED-CP-PCT    TO GFCTVA-PEXCED-CP-PCT.
           MOVE GFCTVB-QDIA-COBR-COMPO  TO GFCTVA-QDIA-COBR-COMPO.
           MOVE GFCTVB-CSEQ-COMP-PCT    TO GFCTVA-CSEC-COMP-PCT.
           MOVE GFCTVB-DINIC-PERIODO    TO GFCTVA-DINI-PER-COMP.
           MOVE GFCTVB-DFIM-PERIODO     TO GFCTVA-DFIM-PER-COMP.
           MOVE GFCTVB-PERIODO-PCT      TO GFCTVA-PERIODO-PCT.
           MOVE GFCTVB-DDIA-COBR-TARIF  TO GFCTVA-DDIA-COBR-TARIF.
           MOVE GFCTVB-CIDTFD-COBR-FERI TO GFCTVA-CIDTFD-COBR-FERI.
           MOVE GFCTVB-CIDTFD-VLR-EXCED TO GFCTVA-CIDTFD-VLR-EXCED.
           MOVE 'E'                     TO GFCTVA-REG-EXCEDENTE.

VI0117     MOVE GFCTVB-PACOTE           TO GFCTVA-PACOTE-EVENTO.

           PERFORM 2000-GRAVAR-EXCED92O.

      *----------------------------------------------------------------*
       1900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     GRAVAR ARQUIVO EXCED92O                                    *
      *----------------------------------------------------------------*
       2000-GRAVAR-EXCED92O            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE FD-REG-EXCED92O       FROM GFCTVA-GFCTB092.

           PERFORM 0700-TESTAR-FS-EXCED92O.

           ADD 1                       TO ACU-GRAVA-EXCED92O.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ATUALIZAR GFCTB034                                         *
      *----------------------------------------------------------------*
       2100-ATUALIZAR-GFCTB034         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             UPDATE DB2PRD.CTRL_PCOTE_MOVTO
                SET QSERVC_UTLZD_PCOTE = :GFCTB034.QSERVC-UTLZD-PCOTE
              WHERE CBCO_DSTNO_MOVTO   = :GFCTB034.CBCO-DSTNO-MOVTO
                AND CAG_DSTNO_MOVTO    = :GFCTB034.CAG-DSTNO-MOVTO
                AND CCTA_DSTNO_MOVTO   = :GFCTB034.CCTA-DSTNO-MOVTO
                AND CTPO_COMPO_PCOTE   = :GFCTB034.CTPO-COMPO-PCOTE
                AND CCOMPO_PCOTE       = :GFCTB034.CCOMPO-PCOTE
                AND DINIC_CTRL_PCOTE   = :GFCTB034.DINIC-CTRL-PCOTE
                AND CTARIF_PCOTE       = :GFCTB034.CTARIF-PCOTE
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'CTRL_PCOTE_MOVTO  '
                                       TO ERR-DBD-TAB
                MOVE 'UPDATE    '      TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0050'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

151009*----------------------------------------------------------------*
151009*     ATUALIZAR GFCTB034 POR QTDE MOVIMENTO                      *
151009*----------------------------------------------------------------*
151009 2150-ATUAL-MV-GFCTB034          SECTION.
151009*----------------------------------------------------------------*
151009
151009     EXEC SQL
151009       UPDATE DB2PRD.CTRL_PCOTE_MOVTO
151009          SET QSERVC_UTLZD_PCOTE = :GFCTB034.QSERVC-UTLZD-PCOTE,
151009              QMOVTO_COMPO_PCOTE = :GFCTB034.QMOVTO-COMPO-PCOTE
151009        WHERE CBCO_DSTNO_MOVTO   = :GFCTB034.CBCO-DSTNO-MOVTO
151009          AND CAG_DSTNO_MOVTO    = :GFCTB034.CAG-DSTNO-MOVTO
151009          AND CCTA_DSTNO_MOVTO   = :GFCTB034.CCTA-DSTNO-MOVTO
151009          AND CTPO_COMPO_PCOTE   = :GFCTB034.CTPO-COMPO-PCOTE
151009          AND CCOMPO_PCOTE       = :GFCTB034.CCOMPO-PCOTE
151009          AND DINIC_CTRL_PCOTE   = :GFCTB034.DINIC-CTRL-PCOTE
151009          AND CTARIF_PCOTE       = :GFCTB034.CTARIF-PCOTE
151009     END-EXEC.
151009
151009     IF  (SQLCODE                NOT EQUAL ZEROS) OR
151009         (SQLWARN0               EQUAL 'W')
151009          MOVE 'DB2'             TO ERR-TIPO-ACESSO
151009          MOVE 'CTRL_PCOTE_MOVTO  '
151009                                 TO ERR-DBD-TAB
151009          MOVE 'UPDATE    '      TO ERR-FUN-COMANDO
151009          MOVE SQLCODE           TO ERR-SQL-CODE
151009          MOVE '0060'            TO ERR-LOCAL
151009          MOVE SPACES            TO ERR-SEGM
151009          PERFORM 9999-TRATAR-ERRO
151009     END-IF.
151009
151009*----------------------------------------------------------------*
151009 2150-99-FIM.                    EXIT.
151009*----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ROTINA PARA TRATAR ISENTO                                  *
      *----------------------------------------------------------------*
       2200-TRATAR-ISENTO              SECTION.
      *----------------------------------------------------------------*

           PERFORM 2300-MONTAR-REG-41-ISENTO.

           PERFORM 2400-GRAVAR-ISENT92O.

           PERFORM 2500-GRAVAR-ISENT41O.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     MONTAR REGISTRO 41 ISENTO                                  *
      *----------------------------------------------------------------*
       2300-MONTAR-REG-41-ISENTO       SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVA-DENVIO-MOVTO-TARIF
                                       TO GFCTV4-DENVIO-MOVTO-TARIF.
RU0125     MOVE GFCTVA-DENVIO-MOVTO-TARIF
                                       TO WRK-CPTCAO-GFCTB041
RU0125     MOVE WRK-CPTCAO-B041        TO GFCTV4-CPTCAO.
           MOVE GFCTVA-CROTNA-ORIGE-MOVTO
                                       TO GFCTV4-CROTNA-ORIGE-MOVTO.
           MOVE GFCTVA-CNRO-ARQ-MOVTO  TO GFCTV4-CNRO-ARQ-MOVTO.
           MOVE GFCTVA-CSEQ-MOVTO      TO GFCTV4-CSEQ-MOVTO.
           MOVE GFCTVA-CBCO-DSTNO-MOVTO
                                       TO GFCTV4-CBCO-DSTNO-MOVTO.
           MOVE GFCTVA-CAG-DSTNO-MOVTO TO GFCTV4-CAG-DSTNO-MOVTO.
           MOVE GFCTVA-CCTA-DSTNO-MOVTO
                                       TO GFCTV4-CCTA-DSTNO-MOVTO.
           MOVE GFCTVA-CPAB-DSTNO-MOVTO
                                       TO GFCTV4-CPAB-DSTNO-MOVTO.
           MOVE GFCTVA-CSERVC-TARIF    TO GFCTV4-CSERVC-TARIF.
           MOVE GFCTVA-DOCOR-EVNTO     TO GFCTV4-DOCOR-EVNTO.
           MOVE GFCTVA-HPREST-SERVC-MOVTO
                                       TO GFCTV4-HPREST-MOVTO.

           EXEC SQL
                SET :WRK-TIMESTAMP = CURRENT TIMESTAMP
           END-EXEC.
           MOVE WRK-TIMESTAMP TO GFCTV4-IDTFD-TIMESTAMP.

           MOVE 3                      TO GFCTV4-CIDTFD-TPO-DESC.
           MOVE 1                      TO GFCTV4-CIDTFD-MOTVO-DESC.
           MOVE ZEROS                  TO GFCTV4-CIDTFD-DESC.
           MOVE 3                      TO GFCTV4-CPARM-DESC.
           MOVE 1                      TO GFCTV4-QEVNTO-DESC.
           MOVE GFCTVA-VTARIF-LIQ-MOVTO
                                       TO GFCTV4-VCONCS-DESC.
           MOVE SPACES                 TO GFCTV4-CCONTR-FLEXZ.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     GRAVAR ARQUIVO ISENT92O                                    *
      *----------------------------------------------------------------*
       2400-GRAVAR-ISENT92O            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

      *----CARIMBA ISENTO COM VALOR ZERADO - 30/01/2007 - CREMM.

CREMM      MOVE 16                     TO GFCTVA-CSIT-EVNTO-RECBD.
CREMM      MOVE ZEROS                  TO GFCTVA-VTARIF-LIQ-MOVTO.

VI0117     MOVE GFCTVB-PACOTE           TO GFCTVA-PACOTE-EVENTO.

           WRITE FD-REG-ISENT92O       FROM GFCTVA-GFCTB092.

           PERFORM 0850-TESTAR-FS-ISENT92O.

           ADD 1                       TO ACU-GRAVA-ISENT92O.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     GRAVAR ARQUIVO ISENT41O                                    *
      *----------------------------------------------------------------*
       2500-GRAVAR-ISENT41O            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE FD-REG-ISENT41O       FROM GFCTV4-GFCTB041.

           PERFORM 0800-TESTAR-FS-ISENT41O.

           ADD 1                       TO ACU-GRAVA-ISENT41O.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     INSERIR GFCTB034                                           *
      *----------------------------------------------------------------*
       2600-INSERIR-GFCTB034           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVA-CBCO-DSTNO-MOVTO
                                       TO CBCO-DSTNO-MOVTO OF GFCTB034.
           MOVE GFCTVA-CAG-DSTNO-MOVTO TO CAG-DSTNO-MOVTO  OF GFCTB034.
           MOVE GFCTVA-CCTA-DSTNO-MOVTO
                                       TO CCTA-DSTNO-MOVTO OF GFCTB034.

           IF  GFCTVB-GRUPO            EQUAL ZEROS
               MOVE 1                  TO CTPO-COMPO-PCOTE OF GFCTB034
               MOVE GFCTVB-COMPO-TARIFA
                                       TO CCOMPO-PCOTE     OF GFCTB034
           ELSE
               MOVE 2                  TO CTPO-COMPO-PCOTE OF GFCTB034
               MOVE GFCTVB-GRUPO
                                       TO CCOMPO-PCOTE     OF GFCTB034
           END-IF.

           MOVE GFCTVB-PACOTE          TO CTARIF-PCOTE     OF GFCTB034.
           MOVE 1                      TO QSERVC-UTLZD-PCOTE
                                                           OF GFCTB034.
           MOVE ZEROS                  TO QMOVTO-COMPO-PCOTE
                                                           OF GFCTB034.

           PERFORM 2650-MONTA-DATA.

           EXEC SQL
             INSERT INTO DB2PRD.CTRL_PCOTE_MOVTO
                  (CBCO_DSTNO_MOVTO,
                   CAG_DSTNO_MOVTO,
                   CCTA_DSTNO_MOVTO,
                   CTPO_COMPO_PCOTE,
                   CCOMPO_PCOTE,
                   DINIC_CTRL_PCOTE,
                   CTARIF_PCOTE,
                   QSERVC_UTLZD_PCOTE,
                   DFIM_CTRL_PCOTE,
                   QMOVTO_COMPO_PCOTE)
               VALUES
                  (:GFCTB034.CBCO-DSTNO-MOVTO,
                   :GFCTB034.CAG-DSTNO-MOVTO,
                   :GFCTB034.CCTA-DSTNO-MOVTO,
                   :GFCTB034.CTPO-COMPO-PCOTE,
                   :GFCTB034.CCOMPO-PCOTE,
                   :GFCTB034.DINIC-CTRL-PCOTE,
                   :GFCTB034.CTARIF-PCOTE,
                   :GFCTB034.QSERVC-UTLZD-PCOTE,
                   :GFCTB034.DFIM-CTRL-PCOTE,
                   :GFCTB034.QMOVTO-COMPO-PCOTE)
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND -803) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'CTRL_PCOTE_MOVTO  '
                                       TO ERR-DBD-TAB
                MOVE 'INSERT    '      TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0070'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-TRATAR-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL  -803
               DISPLAY
               '********************* GFCT1838 **********************'
               DISPLAY
               '**                                                 **'
               DISPLAY
               '** AGENC     CONTA  TIPO  COMPONENTE   DT-INICIO   **'
               DISPLAY
               '** ' CAG-DSTNO-MOVTO OF GFCTB034
                       '   ' CCTA-DSTNO-MOVTO OF GFCTB034
                                 '   ' CTPO-COMPO-PCOTE OF GFCTB034
                                     '       ' CCOMPO-PCOTE OF GFCTB034
                                                 '    '
                                  DINIC-CTRL-PCOTE OF GFCTB034 '   **'
               DISPLAY
               '**  JA  EXISTENTES NA  BASE  GFCTB034              **'
               DISPLAY
               '**                                                 **'
               DISPLAY
               '********************* GFCT1838 **********************'
           END-IF.

      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

151009*----------------------------------------------------------------*
151009*     INSERIR GFCTB034 COM QTDE DE MOVIMENTO                     *
151009*----------------------------------------------------------------*
151009 2620-INSERIR-MV-GFCTB034        SECTION.
151009*----------------------------------------------------------------*
151009
151009     MOVE GFCTVA-CBCO-DSTNO-MOVTO
151009                                 TO CBCO-DSTNO-MOVTO OF GFCTB034.
151009     MOVE GFCTVA-CAG-DSTNO-MOVTO TO CAG-DSTNO-MOVTO  OF GFCTB034.
151009     MOVE GFCTVA-CCTA-DSTNO-MOVTO
151009                                 TO CCTA-DSTNO-MOVTO OF GFCTB034.
151009
151009     IF  GFCTVB-GRUPO            EQUAL ZEROS
151009         MOVE 1                  TO CTPO-COMPO-PCOTE OF GFCTB034
151009         MOVE GFCTVB-COMPO-TARIFA
151009                                 TO CCOMPO-PCOTE     OF GFCTB034
151009     ELSE
151009         MOVE 2                  TO CTPO-COMPO-PCOTE OF GFCTB034
151009         MOVE GFCTVB-GRUPO
151009                                 TO CCOMPO-PCOTE     OF GFCTB034
151009     END-IF.
151009
151009     MOVE GFCTVB-PACOTE          TO CTARIF-PCOTE     OF GFCTB034.
151009     MOVE 1                      TO QSERVC-UTLZD-PCOTE
151009                                                     OF GFCTB034.
151009     MOVE 1                      TO QMOVTO-COMPO-PCOTE
151009                                                     OF GFCTB034.
151009
151009     PERFORM 2650-MONTA-DATA.
151009
151009     EXEC SQL
151009       INSERT INTO DB2PRD.CTRL_PCOTE_MOVTO
151009            (CBCO_DSTNO_MOVTO,
151009             CAG_DSTNO_MOVTO,
151009             CCTA_DSTNO_MOVTO,
151009             CTPO_COMPO_PCOTE,
151009             CCOMPO_PCOTE,
151009             DINIC_CTRL_PCOTE,
151009             CTARIF_PCOTE,
151009             QSERVC_UTLZD_PCOTE,
151009             DFIM_CTRL_PCOTE,
151009             QMOVTO_COMPO_PCOTE)
151009         VALUES
151009            (:GFCTB034.CBCO-DSTNO-MOVTO,
151009             :GFCTB034.CAG-DSTNO-MOVTO,
151009             :GFCTB034.CCTA-DSTNO-MOVTO,
151009             :GFCTB034.CTPO-COMPO-PCOTE,
151009             :GFCTB034.CCOMPO-PCOTE,
151009             :GFCTB034.DINIC-CTRL-PCOTE,
151009             :GFCTB034.CTARIF-PCOTE,
151009             :GFCTB034.QSERVC-UTLZD-PCOTE,
151009             :GFCTB034.DFIM-CTRL-PCOTE,
151009             :GFCTB034.QMOVTO-COMPO-PCOTE)
151009     END-EXEC.
151009
151009     IF  (SQLCODE                NOT EQUAL ZEROS AND -803) OR
151009         (SQLWARN0               EQUAL 'W')
151009          MOVE 'DB2'             TO ERR-TIPO-ACESSO
151009          MOVE 'CTRL_PCOTE_MOVTO  '
151009                                 TO ERR-DBD-TAB
151009          MOVE 'INSERT    '      TO ERR-FUN-COMANDO
151009          MOVE SQLCODE           TO ERR-SQL-CODE
151009          MOVE '0080'            TO ERR-LOCAL
151009          MOVE SPACES            TO ERR-SEGM
151009          PERFORM 9999-TRATAR-ERRO
151009     END-IF.
151009
151009     IF  SQLCODE                 EQUAL  -803
151009         DISPLAY
151009         '********************* GFCT1838 **********************'
151009         DISPLAY
151009         '**                                                 **'
151009         DISPLAY
151009         '** AGENC     CONTA  TIPO  COMPONENTE   DT-INICIO   **'
151009         DISPLAY
151009         '** ' CAG-DSTNO-MOVTO OF GFCTB034
151009                 '   ' CCTA-DSTNO-MOVTO OF GFCTB034
151009                           '   ' CTPO-COMPO-PCOTE OF GFCTB034
151009                               '       ' CCOMPO-PCOTE OF GFCTB034
151009                                           '    '
151009                            DINIC-CTRL-PCOTE OF GFCTB034 '   **'
151009         DISPLAY
151009         '**  JA  EXISTENTES NA  BASE  GFCTB034              **'
151009         DISPLAY
151009         '**                                                 **'
151009         DISPLAY
151009         '********************* GFCT1838 **********************'
151009     END-IF.
151009
151009*----------------------------------------------------------------*
151009 2600-99-FIM.                    EXIT.
151009*----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ROTINA PARA IDENTIFICAR DATA FIM                           *
      *----------------------------------------------------------------*
       2650-MONTA-DATA                 SECTION.
      *----------------------------------------------------------------*

           MOVE '  .  .    '               TO WRK-DATA-DB2.
           MOVE 1                          TO WRK-DD-DB2.
           MOVE GFCTVA-DOCOR-EVNTO         TO WRK-DATA-X10.
           MOVE WRK-MES-X10                TO WRK-MM-DB2.
           MOVE WRK-ANO-X10                TO WRK-AAAA-DB2.
           MOVE WRK-DATA-DB2           TO DINIC-CTRL-PCOTE OF GFCTB034.

           MOVE '  .  .    '               TO WRK-DATA-DB2.
           MOVE GFCTVA-DOCOR-EVNTO         TO WRK-DATA-X10.
           MOVE WRK-MES-X10                TO WRK-MM-DB2.
           MOVE WRK-ANO-X10                TO WRK-AAAA-DB2.

           IF (WRK-MM-DB2              EQUAL 01 OR 03 OR 05 OR 07 OR
                                             08 OR 10 OR 12)
              MOVE 31                      TO WRK-DD-DB2
           ELSE
              IF (WRK-MM-DB2           EQUAL 04 OR 06 OR
                                             09 OR 11)
                  MOVE 30                  TO WRK-DD-DB2
              ELSE
                 IF WRK-MM-DB2         EQUAL 02
                    MOVE    WRK-AAAA-DB2  TO  WRK-AAAA-DB2-C
                    DIVIDE  WRK-AAAA-DB2-C    BY  4
                                       GIVING WRK-INTEIRO-C
                                       REMAINDER WRK-RESTO-C
                    IF  WRK-RESTO-C    NOT EQUAL ZEROS
                        MOVE 28            TO WRK-DD-DB2
                    ELSE
                        MOVE 29            TO WRK-DD-DB2
                    END-IF
                 END-IF
              END-IF
           END-IF.

           MOVE WRK-DATA-DB2           TO DFIM-CTRL-PCOTE  OF GFCTB034.

      *----------------------------------------------------------------*
       2650-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     INSERIR GFCTB034 - TIPO COMPONENTE PACOTE 4                *
      *----------------------------------------------------------------*
       2700-INSERIR-GFCTB034-TIPO-4    SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVA-CBCO-DSTNO-MOVTO
                                       TO CBCO-DSTNO-MOVTO  OF GFCTB034.
           MOVE GFCTVA-CAG-DSTNO-MOVTO TO CAG-DSTNO-MOVTO   OF GFCTB034.
           MOVE GFCTVA-CCTA-DSTNO-MOVTO
                                       TO CCTA-DSTNO-MOVTO  OF GFCTB034.
           MOVE 4                      TO CTPO-COMPO-PCOTE  OF GFCTB034
           MOVE GFCTVB-COMPO-TARIFA    TO CCOMPO-PCOTE      OF GFCTB034
           MOVE GFCTVB-PACOTE          TO CTARIF-PCOTE      OF GFCTB034.
           MOVE 1                      TO QSERVC-UTLZD-PCOTE
                                                            OF GFCTB034.
           MOVE ZEROS                  TO QMOVTO-COMPO-PCOTE
                                                            OF GFCTB034.

           EXEC SQL
             INSERT
               INTO DB2PRD.CTRL_PCOTE_MOVTO
                   (CBCO_DSTNO_MOVTO,
                    CAG_DSTNO_MOVTO,
                    CCTA_DSTNO_MOVTO,
                    CTPO_COMPO_PCOTE,
                    CCOMPO_PCOTE,
                    DINIC_CTRL_PCOTE,
                    CTARIF_PCOTE,
                    QSERVC_UTLZD_PCOTE,
                    DFIM_CTRL_PCOTE,
                    QMOVTO_COMPO_PCOTE)
             VALUES
                   (:GFCTB034.CBCO-DSTNO-MOVTO,
                    :GFCTB034.CAG-DSTNO-MOVTO,
                    :GFCTB034.CCTA-DSTNO-MOVTO,
                    :GFCTB034.CTPO-COMPO-PCOTE,
                    :GFCTB034.CCOMPO-PCOTE,
                    :GFCTB034.DINIC-CTRL-PCOTE,
                    :GFCTB034.CTARIF-PCOTE,
                    :GFCTB034.QSERVC-UTLZD-PCOTE,
                    :GFCTB034.DFIM-CTRL-PCOTE,
                    :GFCTB034.QMOVTO-COMPO-PCOTE)
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND -803) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'CTRL_PCOTE_MOVTO'
                                       TO ERR-DBD-TAB
                MOVE 'INSERT'          TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0090'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM

                PERFORM 9999-TRATAR-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL  -803
               DISPLAY
               '********************* GFCT1838 **********************'
               DISPLAY
               '**                                                 **'
               DISPLAY
               '** AGENC     CONTA  TIPO  COMPONENTE   DT-INICIO   **'
               DISPLAY
               '** ' CAG-DSTNO-MOVTO OF GFCTB034
                       '   ' CCTA-DSTNO-MOVTO OF GFCTB034
                                 '   ' CTPO-COMPO-PCOTE OF GFCTB034
                                     '       ' CCOMPO-PCOTE OF GFCTB034
                                                 '    '
                                  DINIC-CTRL-PCOTE OF GFCTB034 '   **'
               DISPLAY
               '**  JA  EXISTENTES NA  BASE  GFCTB034              **'
               DISPLAY
               '**                                                 **'
               DISPLAY
               '********************* GFCT1838 **********************'
           END-IF.

      *----------------------------------------------------------------*
       2700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ROTINA DO CHECK POINT                                      *
      *----------------------------------------------------------------*
       2800-CHECK-POINT                SECTION.
      *----------------------------------------------------------------*

           INITIALIZE CKRS01-INTERFACE.

           MOVE 'DB2 '                 TO CK01-ID-DB2.
           MOVE 'P'                    TO CK01-FUNCAO.
           MOVE ACU-LIDOS-COMPCLII     TO WRK-LIDOS-COMPCLII.
           MOVE ACU-LIDOS-EVCADSAI     TO WRK-LIDOS-EVCADSAI.
           MOVE ACU-GRAVA-EXCED92O     TO WRK-GRAVA-EXCED92O.
           MOVE ACU-GRAVA-ISENT41O     TO WRK-GRAVA-ISENT41O.
310709     MOVE ACU-GRAVA-ST18T41O     TO WRK-GRAVA-ST18T41O.
           MOVE ACU-GRAVA-ISENT92O     TO WRK-GRAVA-ISENT92O.
           MOVE ACU-GRAVA-EXCEDRES     TO WRK-GRAVA-EXCEDRES.
           MOVE ACU-GRAVA-STGRPSRV     TO WRK-GRAVA-STGRPSRV.
310709     MOVE ACU-GRAVA-ST18T92O     TO WRK-GRAVA-ST18T92O.
           MOVE ACU-GRAVA-EVNBATEO     TO WRK-GRAVA-EVNBATEO.
CREMM      MOVE WRK-AREA-RESTART       TO CK01-AREA-RESTART.
           MOVE LENGTH OF WRK-AREA-RESTART
                                       TO CK01-TAM-AREA-RESTART.

           CALL WRK-CKRS0100           USING CKRS01-INTERFACE.

           IF  CK01-CODIGO-RETORNO     NOT EQUAL ZEROS
               DISPLAY '****  NO PROCESSAMENTO  ****'
               PERFORM 0300-TRATAR-ERRO-RESTART
           END-IF.

      *----------------------------------------------------------------*
       2800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     GRAVAR ARQUIVO EVNBATEO                                    *
      *----------------------------------------------------------------*
       2900-GRAVAR-EVNBATEO            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE FD-REG-EVNBATEO       FROM GFCTVA-GFCTB092.

           PERFORM 1000-TESTAR-FS-EVNBATEO.

           ADD 1                       TO ACU-GRAVA-EVNBATEO.

      *----------------------------------------------------------------*
       2900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     FINALIZAR CHECK POINT                                      *
      *----------------------------------------------------------------*
       3000-FINALIZA-CHECK-POINT       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE CKRS01-INTERFACE.

           MOVE 'DB2 '                 TO CK01-ID-DB2.
           MOVE 'F'                    TO CK01-FUNCAO.

           CALL WRK-CKRS0100           USING CKRS01-INTERFACE.

           IF  CK01-CODIGO-RETORNO     NOT EQUAL ZEROS
               DISPLAY '****  NA FINALIZACAO  ****'
               PERFORM 0300-TRATAR-ERRO-RESTART
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ROTINA PARA TRATAR EXCEDENTE POR RESTRICAO DE TARIFAS      *
      *       DE GRUPO DE SERVICO                                      *
      *----------------------------------------------------------------*
       3800-TRATAR-EXCED-RESTR         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHAVE-EVENTO        NOT EQUAL WRK-CHAVE-W-EVENTO
               MOVE QSERVC-UTLZD-PCOTE OF GFCTB034
                                       TO WRK-QTDE-AUX
               MOVE 1                  TO WRK-QTDE-EXC-DIA
           ELSE
               ADD 1                   TO WRK-QTDE-EXC-DIA
           END-IF.

           MOVE WRK-QTDE-EXC-DIA        TO GFCTVA-QTD-EXC-DIA.
           MOVE WRK-QTDE-AUX-5          TO GFCTVA-QTD-EXC-ACU.
           MOVE WRK-QTDE-OBTIDA         TO GFCTVA-QTD-FQ-COMPON.
           MOVE GFCTVB-CPER-COBR-COMPO  TO GFCTVA-CPER-COBR-COMPO.
           MOVE GFCTVB-VEXCED-CP-PCT    TO GFCTVA-VEXCED-CP-PCT.
           MOVE GFCTVB-PEXCED-CP-PCT    TO GFCTVA-PEXCED-CP-PCT.
           MOVE GFCTVB-QDIA-COBR-COMPO  TO GFCTVA-QDIA-COBR-COMPO.
           MOVE GFCTVB-CSEQ-COMP-PCT    TO GFCTVA-CSEC-COMP-PCT.
           MOVE GFCTVB-DINIC-PERIODO    TO GFCTVA-DINI-PER-COMP.
           MOVE GFCTVB-DFIM-PERIODO     TO GFCTVA-DFIM-PER-COMP.
           MOVE GFCTVB-PERIODO-PCT      TO GFCTVA-PERIODO-PCT.
           MOVE GFCTVB-DDIA-COBR-TARIF  TO GFCTVA-DDIA-COBR-TARIF.
           MOVE GFCTVB-CIDTFD-COBR-FERI TO GFCTVA-CIDTFD-COBR-FERI.
           MOVE GFCTVB-CIDTFD-VLR-EXCED TO GFCTVA-CIDTFD-VLR-EXCED.
           MOVE 'R'                     TO GFCTVA-REG-EXCEDENTE.

VI0117     MOVE GFCTVB-PACOTE           TO GFCTVA-PACOTE-EVENTO.

2603       IF  GFCTVA-VEXCED-CP-PCT    EQUAL ZEROS
2603       AND GFCTVA-PEXCED-CP-PCT    EQUAL ZEROS
2603           NEXT SENTENCE
2603       ELSE
2603           IF  GFCTVA-VEXCED-CP-PCT
2603                                   NOT EQUAL ZEROS
2603               MOVE GFCTVA-VEXCED-CP-PCT
2603                                   TO GFCTVA-VTARIF-LIQ-MOVTO
2603                                      GFCTVA-VTARIF-BRUTO-MOVTO
2603                                      GFCTVA-VTARIF-CADTR-SIST
2603           ELSE
2603               PERFORM 3850-CALCULA-PERCENTUAL
2603               MOVE WRK-VLR-BRUTO  TO GFCTVA-VTARIF-LIQ-MOVTO
2603                                      GFCTVA-VTARIF-BRUTO-MOVTO
2603                                     GFCTVA-VTARIF-CADTR-SIST
2603           END-IF
2603       END-IF.

           PERFORM 3900-GRAVAR-EXCEDRES.

      *----------------------------------------------------------------*
       3800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
2603  *     TRATA VALORES                                              *
2603  *----------------------------------------------------------------*
2603   3850-CALCULA-PERCENTUAL         SECTION.
2603  *----------------------------------------------------------------*
2603
2603       COMPUTE WRK-VLR-BRUTO       =  ((GFCTVA-VTARIF-BRUTO-MOVTO *
2603                                       GFCTVA-PEXCED-CP-PCT) / 100).
2603
2603  *----------------------------------------------------------------*
2603   3850-99-FIM.                    EXIT.
2603  *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     GRAVAR ARQUIVO EXCEDRES                                    *
      *----------------------------------------------------------------*
       3900-GRAVAR-EXCEDRES            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE FD-REG-EXCEDRES       FROM GFCTVA-GFCTB092.

           PERFORM 0750-TESTAR-FS-EXCEDRES.

           ADD 1                       TO ACU-GRAVA-EXCEDRES.

      *----------------------------------------------------------------*
       3900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ROTINA PARA TRATAR ISENTO - SITUACAO 16                    *
      *      TRATAMENTO PARA GRUPO DE SERVICOS                         *
      *----------------------------------------------------------------*
       4100-GRAVAR-STGRPSRV            SECTION.
      *----------------------------------------------------------------*

VI0117     MOVE GFCTVB-PACOTE          TO GFCTVA-PACOTE-EVENTO.

           MOVE    WRK-GRAVACAO        TO        WRK-OPERACAO.
           WRITE   FD-REG-STGRPSRV    FROM       GFCTVA-GFCTB092.
           PERFORM 0950-TESTAR-FS-STGRPSRV.
           ADD     1                   TO        ACU-GRAVA-STGRPSRV.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

310709*----------------------------------------------------------------*
310709*     ROTINA PARA TRATAR ISENTO - SITUACAO 18                    *
310709*      TRATAMENTO PARA GRUPO DE SERVICOS                         *
310709*----------------------------------------------------------------*
310709 4200-TRATAR-ISENTO18            SECTION.
310709*----------------------------------------------------------------*
310709
310709     PERFORM 4300-MONTAR-REG-41-ST18.
310709
310709     PERFORM 4400-GRAVAR-ST18T92O.
310709
310709     PERFORM 4500-GRAVAR-ST18T41O.
310709
310709*----------------------------------------------------------------*
310709 4200-99-FIM.                    EXIT.
310709*----------------------------------------------------------------*

310709*----------------------------------------------------------------*
310709*     ROTINA PARA TRATAR ISENTO - SITUACAO 18 - REGISTRO TAB 41  *
310709*      TRATAMENTO PARA GRUPO DE SERVICOS                         *
310709*----------------------------------------------------------------*
310709 4300-MONTAR-REG-41-ST18         SECTION.
310709*----------------------------------------------------------------*
310709
310709     MOVE GFCTVA-DENVIO-MOVTO-TARIF
310709                                 TO GFCTV4-DENVIO-MOVTO-TARIF.
310709     MOVE GFCTVA-CROTNA-ORIGE-MOVTO
310709                                 TO GFCTV4-CROTNA-ORIGE-MOVTO.
310709     MOVE GFCTVA-CNRO-ARQ-MOVTO  TO GFCTV4-CNRO-ARQ-MOVTO.
310709     MOVE GFCTVA-CSEQ-MOVTO      TO GFCTV4-CSEQ-MOVTO.
310709     MOVE GFCTVA-CBCO-DSTNO-MOVTO
310709                                 TO GFCTV4-CBCO-DSTNO-MOVTO.
310709     MOVE GFCTVA-CAG-DSTNO-MOVTO TO GFCTV4-CAG-DSTNO-MOVTO.
310709     MOVE GFCTVA-CCTA-DSTNO-MOVTO
310709                                 TO GFCTV4-CCTA-DSTNO-MOVTO.
310709     MOVE GFCTVA-CPAB-DSTNO-MOVTO
310709                                 TO GFCTV4-CPAB-DSTNO-MOVTO.
310709     MOVE GFCTVA-CSERVC-TARIF    TO GFCTV4-CSERVC-TARIF.
310709     MOVE GFCTVA-DOCOR-EVNTO     TO GFCTV4-DOCOR-EVNTO.
310709     MOVE GFCTVA-HPREST-SERVC-MOVTO
310709                                 TO GFCTV4-HPREST-MOVTO.
310709
310709     EXEC SQL
310709          SET :WRK-TIMESTAMP = CURRENT TIMESTAMP
310709     END-EXEC.
           MOVE WRK-TIMESTAMP TO GFCTV4-IDTFD-TIMESTAMP.
310709
310709     MOVE 3                      TO GFCTV4-CIDTFD-TPO-DESC.
310709     MOVE 1                      TO GFCTV4-CIDTFD-MOTVO-DESC.
310709     MOVE ZEROS                  TO GFCTV4-CIDTFD-DESC.
310709     MOVE 3                      TO GFCTV4-CPARM-DESC.
310709     MOVE 1                      TO GFCTV4-QEVNTO-DESC.
310709     MOVE GFCTVA-VTARIF-LIQ-MOVTO
310709                                 TO GFCTV4-VCONCS-DESC.
310709     MOVE SPACES                 TO GFCTV4-CCONTR-FLEXZ.
310709
310709*----------------------------------------------------------------*
310709 4300-99-FIM.                    EXIT.
310709*----------------------------------------------------------------*

310709*----------------------------------------------------------------*
310709*     ROTINA PARA TRATAR ISENTO - SITUACAO 18 - REGISTRO TAB 41  *
310709*      TRATAMENTO PARA GRUPO DE SERVICOS                         *
310709*----------------------------------------------------------------*
310709 4400-GRAVAR-ST18T92O            SECTION.
310709*----------------------------------------------------------------*
310709
310709     MOVE 18                     TO GFCTVA-CSIT-EVNTO-RECBD.
310709     MOVE ZEROS                  TO GFCTVA-VTARIF-LIQ-MOVTO.

VI0117     MOVE GFCTVB-PACOTE           TO GFCTVA-PACOTE-EVENTO.

310709     MOVE    WRK-GRAVACAO        TO        WRK-OPERACAO.
310709     WRITE   FD-REG-ST18T92O    FROM       GFCTVA-GFCTB092.
310709     PERFORM 0970-TESTAR-FS-ST18T92O.
310709     ADD     1                   TO        ACU-GRAVA-ST18T92O.
310709
310709*----------------------------------------------------------------*
310709 4400-99-FIM.                    EXIT.
310709*----------------------------------------------------------------*
310709

310709*----------------------------------------------------------------*
310709*     ROTINA PARA TRATAR ISENTO - SITUACAO 18 - REGISTRO TAB 41  *
310709*      TRATAMENTO PARA GRUPO DE SERVICOS                         *
310709*----------------------------------------------------------------*
310709 4500-GRAVAR-ST18T41O            SECTION.
310709*----------------------------------------------------------------*
310709
310709     MOVE    WRK-GRAVACAO        TO        WRK-OPERACAO.
310709     WRITE   FD-REG-ST18T41O    FROM       GFCTV4-GFCTB041.
310709     PERFORM 0820-TESTAR-FS-ST18T41O.
310709     ADD     1                   TO        ACU-GRAVA-ST18T41O.
310709
310709*----------------------------------------------------------------*
310709 4500-99-FIM.                    EXIT.
310709*----------------------------------------------------------------*
310709
      *----------------------------------------------------------------*
      *     PROCEDIMENTOS FINAIS                                       *
      *----------------------------------------------------------------*
       9000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 9100-EMITIR-DISPLAY.

           CLOSE COMPCLII
                 EVCADSAI
                 EXCED92O
                 ISENT41O
310709           ST18T41O
                 ISENT92O
                 EXCEDRES
                 STGRPSRV
310709           ST18T92O
                 EVNBATEO.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FILE-STATUS.

           PERFORM 3000-FINALIZA-CHECK-POINT.

           STOP RUN.

      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ESTATISTICAS DE PROCESSAMENTO                               *
      *----------------------------------------------------------------*
       9100-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS-COMPCLII     TO WRK-MASCARA.

           DISPLAY '******************** GFCT1838 '
                   '********************'.
           DISPLAY '*                             '
                   '                   *'.
           DISPLAY '*        PROGRAMA ENCERRADO CO'
                   'M SUCESSO          *'.
           DISPLAY '* ============================'
                   '===============    *'.
           DISPLAY '*   PROCESSAMENTO DE : ' DPROCM-ATUAL OF GFCTB0A1
                   '                *'.
           DISPLAY '* ============================'
                   '=====              *'.
           DISPLAY '*   TOTAL DE LIDOS EM COMPCLII.: '
                   WRK-MASCARA '  *'.

           MOVE ACU-LIDOS-EVCADSAI     TO WRK-MASCARA.

           DISPLAY '*   TOTAL DE LIDOS EM EVCADSAI.: '
                   WRK-MASCARA '  *'.

           MOVE ACU-GRAVA-EXCED92O     TO WRK-MASCARA.

           DISPLAY '*   TOTAL DE GRAVADOS EXCED92O.: '
                   WRK-MASCARA '  *'.

           MOVE ACU-GRAVA-ISENT92O     TO WRK-MASCARA.

           DISPLAY '*   TOTAL DE GRAVADOS ISENT92O.: '
                   WRK-MASCARA '  *'.

           MOVE ACU-GRAVA-ISENT41O     TO WRK-MASCARA.

           DISPLAY '*   TOTAL DE GRAVADOS ISENT41O.: '
                   WRK-MASCARA '  *'.

310709     MOVE ACU-GRAVA-ST18T41O     TO WRK-MASCARA.
310709
310709     DISPLAY '*   TOTAL DE GRAVADOS ST18T41O.: '
310709             WRK-MASCARA '  *'.

           MOVE ACU-GRAVA-EXCEDRES     TO WRK-MASCARA.

           DISPLAY '*   TOTAL DE GRAVADOS EXCEDRES.: '
                   WRK-MASCARA '  *'.

           MOVE ACU-GRAVA-STGRPSRV     TO WRK-MASCARA.

           DISPLAY '*   TOTAL DE GRAVADOS STGRPSRV.: '
                   WRK-MASCARA '  *'.

310709     MOVE ACU-GRAVA-ST18T92O     TO WRK-MASCARA.
310709
310709     DISPLAY '*   TOTAL DE GRAVADOS ST18T92O.: '
310709             WRK-MASCARA '  *'.

           MOVE ACU-GRAVA-EVNBATEO     TO WRK-MASCARA.

           DISPLAY '*   TOTAL DE GRAVADOS EVNBATEO.: '
                   WRK-MASCARA '  *'.
           DISPLAY '*                             '
                   '                   *'.
           DISPLAY '*                   TERMINO NO'
                   'RMAL               *'.
           DISPLAY '*                             '
                   '                   *'.
           DISPLAY '******************** GFCT1838 '
                   '********************'.

      *----------------------------------------------------------------*
       9100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ROTINA TRATAR ERRO                                         *
      *----------------------------------------------------------------*
       9999-TRATAR-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1838'             TO ERR-PGM.

           IF  ERR-TIPO-ACESSO         EQUAL 'DB2'
               CALL 'POOL7100'         USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA
           ELSE
               CALL 'POOL7100'         USING WRK-BATCH
                                             ERRO-AREA
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
