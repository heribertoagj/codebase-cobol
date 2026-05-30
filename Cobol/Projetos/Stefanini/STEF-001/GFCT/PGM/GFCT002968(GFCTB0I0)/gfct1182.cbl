      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1182.
       AUTHOR.     JEFERSON PAULO DALPONTE.
      *================================================================*
      *                    C P M  -  S I S T E M A S.                  *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1182                                    *
      *    PROGRAMADOR.:   JEFERSON PAULO DALPONTE - CPM PATO BRANCO   *
      *    ANALISTA CPM:   KHARUZO INOCENCIO LEITE - CPM PATO BRANCO   *
      *    ANALISTA....:   WAGNER SILVA            - PROCKWORK GP. 50  *
      *    DATA........:   17/02/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   ATUALIZAR REGISTROS COBRADOS - COBRANCA     *
      *      CONSOLIDADA.                                              *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                    ATUALIZA                  I#GFCTIL          *
      *                    DETALHES                  I#GFCTIL          *
      *                    CTRLDATA                  I#GFCT06          *
      *                    SAIDETAL                  I#GFCTIL          *
      *                    FITCONTB                  I#GFCTJH          *
      *                    OCOR1182                  I#GFCTIL          *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                TABLE                       INCLUDE/BOOK        *
      *                    DB2PRD.MOVTO_EVNTO_CRRTT  GFCTB092          *
      *                    DB2PRD.TDEB_PCIAL_TARIF   GFCTB0H7
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    POL7100C - AREA DE COMUNICACAO COM A POOL7100               *
      *    I#GFCT0M - AREA DE COMUNICACAO DE ERROS                     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL1050 - MODULO PARA CONEXAO COM DB2                      *
      *    POOL7100 - MODULO PARA TRATAMENTO DE ERROS                  *
      *    GFCT5525 - MODULO PARA ATUALIZAR SITUACAO EVENTO            *
      *================================================================*
      *                       A L T E R A C A O                        *
      *================================================================*
      *    ANALISTA....:   WAGNER SILVA            - PROCKWORK GP. 50  *
      *    DATA........:   22/11/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   INIBIR ATUALIZACAO DO CPO DAGNDA-DEB-MOVTO  *
      *================================================================*
      *    PROGRAMA....:   ALTERACAO                                   *
      *    ANALISTA....:   WAGNER SILVA            - PROCKWORK GP. 50  *
      *    DATA........:   17/01/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   ATUALIZAR AG/CTA DEBITO OU CENTRALIZADORA,  *
      *                    EM CASO DE TRAG NO MORA/POUP                *
      *----------------------------------------------------------------*
070705*================================================================*
070705*                        A L T E R A C A O                       *
070705*================================================================*
070705*    ANALISTA....:   VINICIUS C. MADUREIRA   - PROCKWORK GP. 50  *
070705*    DATA........:   05/07/2007                                  *
070705*----------------------------------------------------------------*
070705*    OBJETIVO....:   ACERTOS DIVERSOS                            *
070705*     01 - ZERAR VLR VINDO DO MORA SE COD-RETORNO = 001 OU 012   *
070705*     02 - MOVER 14 P/ SITUACAO-EVENTO  EM CASO DE ERRO          *
070705*     03 - DISPLAY SE REG ATUALIZA NAO ENCONTRADO NO DETALHES    *
070705*          LIMITADO A 1000 OCORRENCIAS                           *
070705*     04 - INCLUIR TRATAMENTO PARA COBR-COD-RETORNO=F RETORNAR   *
070705*          SITUACAO EVENTO DA 92 P/ SITUACAO ANTERIOR A COBRANCA *
070705*                                                                *
070705*----------------------------------------------------------------*

070710*================================================================*
070710*                        A L T E R A C A O                       *
070710*================================================================*
070710*    ANALISTA....:   VINICIUS C. MADUREIRA   - PROCKWORK GP. 50  *
070710*    DATA........:   10/07/2007                                  *
070710*----------------------------------------------------------------*
070710*    OBJETIVO....:   ACERTOS DIVERSOS                            *
070710*     05 - SUBSTITUIR ACESSO DA GFCT5525 POR UPDATE E ARQUIVO    *
070710*     06 - INCLUIR ARQUIVO 'ARQDEBIT' CONTENDO INFORMACOES SOBRE *
070710*          REGISTROS COBRADOS                                    *
070710*     07 - DEIXAR DE GRAVAR GFCTB0H4 EM CASO DE REPIQUE QUANDO   *
070710*     VALOR DEBITADO IGUAL A 0.                                  *
070710*     08 - FORMATAR DATA DE OCORRENCIA DO EVENTO NA I#GFCTZJ     *
070710*     09 - SE FIM DO REPIQUE SEM COBRAR NADA, STATUS = 12        *
070710*     10 - GRAVAR TAB H7 SE COBRANCA TOTAL E VL DEBITAR DIFERENTE*
070710*          DO VL LIQUIDO DA 92 (FIM DE REPIQUE C/ COBRACA TOTAL) *
070710*----------------------------------------------------------------*
071129*================================================================*
071129*=                      A L T E R A C A O                       =*
071129*================================================================*
071129*    ANALISTA....:   VINICIUS C. MADUREIRA - PROCWORK GP. 50     *
071129*    DATA........:   29/11/2007                                  *
071129*----------------------------------------------------------------*
071129*    OBJETIVO....:   INCLUIR ARQUIVO CONTENDO INFORMACOES SOBRE  *
071129*                    REGISTROS COBRADOS                          *
071129*================================================================*
071130*================================================================*
071130*=                      A L T E R A C A O                       =*
071130*================================================================*
071130*   ANALISTA....:   VINICIUS C. MADUREIRA - PROCWORK GP. 50      *
071130*   DATA........:   30/11/2007                                   *
071130*----------------------------------------------------------------*
071130*   OBJETIVOS...:                                                *
071130*    SUBSTITUIR UPDATE DA TABELA GFCTBO92 POR GERACAO DE ARQUIVO *
071130*    SUBSTITUIR INSERT DA TABELA GFCTBOH7 POR GERACAO DE ARQUIVO *
071130*================================================================*
030408*================================================================*
030408*=                      A L T E R A C A O                       =*
030408*================================================================*
030408*   ANALISTA....:   WAGNER SILVAA - PROCWORK GP. 50              *
030408*   DATA........:   03/04/2008                                   *
030408*----------------------------------------------------------------*
030408*   OBJETIVOS...:                                                *
030408*    SUBSTITUIR REGISTRO DE ATUALIZA PELO DE DETALHES NA GERACAO *
030408*    DO ARQCOBR2                                                 *
030408*================================================================*

080603*================================================================*
080603*        S O N D A  P R O C W O R K  -  A L T E R A C A O        *
080603*----------------------------------------------------------------*
080603*    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP50 *
080603*    ANALISTA....:   WAGNER SILVA                - SONDPROC/GP50 *
080603*    DATA........:   03/06/2008                                  *
080603*----------------------------------------------------------------*
080603*    OBJETIVO....:   RETIRADO SITUACAO NO VALOR MINIMO E GRAVA   *
080603*                    CONTABIL E INCLUIDO REPIQUE                 *
080603*================================================================*

090604*================================================================*
090604*        S O N D A  P R O C W O R K  -  A L T E R A C A O        *
090604*----------------------------------------------------------------*
090604*    ANALISTA....:   WAGNER SILVA                - SONDPROC/GP50 *
090604*    DATA........:   04/06/2009                                  *
090604*----------------------------------------------------------------*
090604*    OBJETIVO....:   ZERAR WRK-VLR-ATUALIZA APOS COBRANCA POR    *
090604*                    VALOR MINIMO                                *
090604*================================================================*

NC1706*================================================================*
NC1706*        S O N D A  P R O C W O R K  -  A L T E R A C A O        *
NC1706*----------------------------------------------------------------*
NC1706*    ANALISTA....:   NELSON LUIZ                 - SONDPROC/GP50 *
NC1706*    DATA........:   17/06/2010                                  *
NC1706*----------------------------------------------------------------*
NC1706*    OBJETIVO....:   INCLUIR A CONSISTENCIA NO PARCIAL-ULT-REPIQ *
NC1706*                    A VALIDACAO DE 1 OR S                       *
NC1706*================================================================*
BI1210*=                      A L T E R A C A O                       =*
BI1210*================================================================*
BI1210*   ANALISTA....:   UBIRAJARA (BIRA) - PROCWORK GP. 50           *
BI1210*   DATA........:   20/12/2010                                   *
BI1210*----------------------------------------------------------------*
BI1210*   OBJETIVOS...:                                                *
BI1210*    MOVIMENTAR O CAMPO (COBR-NUM-PARC) DE (DETALHES) PARA       *
BI1210*    (GFCT67-NUM-PARC) NA GRAVACAO DE ARQATU92.                  *
BI1210*================================================================*

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

           SELECT ATUALIZA  ASSIGN     TO UT-S-ATUALIZA
           FILE STATUS                 IS WRK-FS-ATUALIZA.

           SELECT DETALHES ASSIGN      TO UT-S-DETALHES
           FILE STATUS                 IS WRK-FS-DETALHES.

           SELECT CTRLDATA ASSIGN      TO UT-S-CTRLDATA
           FILE STATUS                 IS WRK-FS-CTRLDATA.

           SELECT SAIDETAL ASSIGN      TO UT-S-SAIDETAL
           FILE STATUS                 IS WRK-FS-SAIDETAL.

           SELECT FITCONTB ASSIGN      TO UT-S-FITCONTB
           FILE STATUS                 IS WRK-FS-FITCONTB.

           SELECT OCOR1182 ASSIGN      TO UT-S-OCOR1182
           FILE STATUS                 IS WRK-FS-OCOR1182.
070710
05         SELECT ATLZB0H4 ASSIGN      TO UT-S-ATLZB0H4
05         FILE STATUS                 IS WRK-FS-ATLZB0H4.
070710
06         SELECT ARQDEBIT  ASSIGN     TO UT-S-ARQDEBIT
06         FILE STATUS                 IS WRK-FS-ARQDEBIT.

071129     SELECT ARQCOBR2  ASSIGN     TO UT-S-ARQCOBR2
071129     FILE STATUS                 IS WRK-FS-ARQCOBR2.

071130     SELECT ARQATU92  ASSIGN     TO UT-S-ARQATU92
071130     FILE STATUS                 IS WRK-FS-ARQATU92.

071130     SELECT ARQATUH7  ASSIGN     TO UT-S-ARQATUH7
071130     FILE STATUS                 IS WRK-FS-ARQATUH7.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  REGISTROS UNIFICADOS COBRADOS PARA ATUALIZACAO NA   *
      *            TAB. GFCTB092.                                      *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  ATUALIZA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       COPY 'I#GFCTIL'.

      *----------------------------------------------------------------*
      *    INPUT:  DETALHES DE REGISTROS ENVIADOS PARA COBRANCA        *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  DETALHES
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       COPY 'I#GFCTIL'.

      *----------------------------------------------------------------*
      *    INPUT:  CONTROLE DE DATAS GFCT                              *
      *            ORG. SEQUENCIAL     -   LRECL   =   100             *
      *----------------------------------------------------------------*

       FD  CTRLDATA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       COPY 'I#GFCT06'.

070710
05    *----------------------------------------------------------------*
05    *    INPUT:  REGISTROS PARA ATUALIZACAO DA TABELA GFCTB0H4       *
05    *            ORG. SEQUENCIAL     -   LRECL   =   050             *
05    *----------------------------------------------------------------*
05
05     FD  ATLZB0H4
05         RECORDING MODE IS F
05         LABEL RECORD IS STANDARD
05         BLOCK CONTAINS  0 RECORDS.
05
05     COPY 'I#GFCTZD'.

070710
06    *----------------------------------------------------------------*
06    *   OUTPUT:  REGISTROS EFETIVAMENTE DEBITADOS                    *
06    *            ORG. SEQUENCIAL     -   LRECL   =   080             *
06    *----------------------------------------------------------------*
06
06     FD  ARQDEBIT
06         RECORDING MODE IS F
06         LABEL RECORD IS STANDARD
06         BLOCK CONTAINS  0 RECORDS.
06
06     COPY 'I#GFCTZJ'.

      *----------------------------------------------------------------*
      *    OUTPUT: DETALHES NAO ENCONTRADOS EM ATUALIZA                *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  SAIDETAL
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       COPY 'I#GFCTIL'.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQ. PARA GERACAO DA FITA4                          *
      *            ORG. SEQUENCIAL     -   LRECL   =   130             *
      *----------------------------------------------------------------*

       FD  FITCONTB
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       COPY 'I#GFCTJH'.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE ERROS DE PROCESSAMENTO                   *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  OCOR1182
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       COPY 'I#GFCTIL'.

071129*----------------------------------------------------------------*
071129*   OUTPUT:  REGISTROS EFETIVAMENTE DEBITADOS                    *
071129*            ORG. SEQUENCIAL     -   LRECL   =   340             *
071129*----------------------------------------------------------------*
071129
071129 FD  ARQCOBR2
071129     RECORDING MODE IS F
071129     LABEL RECORD IS STANDARD
071129     BLOCK CONTAINS  0 RECORDS.
071129
071129 COPY 'I#GFCTMW'.

071130*----------------------------------------------------------------*
071130*   OUTPUT:  REGISTROS PARA ATUALIZACAO NA TABELA GFCTB092       *
071130*            ORG. SEQUENCIAL     -   LRECL   =   090             *
071130*----------------------------------------------------------------*
071130
071130 FD  ARQATU92
071130     RECORDING MODE IS F
071130     LABEL RECORD IS STANDARD
071130     BLOCK CONTAINS  0 RECORDS.
071130
071130 COPY 'I#GFCT67'.

071130*----------------------------------------------------------------*
071130*   OUTPUT:  REGISTROS PARA INSERIR  NA TABELA GFCTB0H7          *
071130*            ORG. SEQUENCIAL     -   LRECL   =   038             *
071130*----------------------------------------------------------------*
071130
071130 FD  ARQATUH7
071130     RECORDING MODE IS F
071130     LABEL RECORD IS STANDARD
071130     BLOCK CONTAINS  0 RECORDS.
071130
071130 COPY 'I#GFCT68'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)          VALUE
           '*** INICIO DA WORKING STORAGE SECTION ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** ACUMULADORES ***'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-ATUALIZA          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-DETALHES          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-CTRLDATA          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAV-SAIDETAL           PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAV-FITCONTB           PIC  9(009) COMP-3  VALUE ZEROS.
070710
06     77  ACU-GRAV-ARQDEBIT           PIC  9(009) COMP-3  VALUE ZEROS.
070710
05     77  ACU-GRAV-ATLZB0H4           PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAV-OCOR1182           PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-DESPREZADOS             PIC  9(009) COMP-3  VALUE ZEROS.
070705
04     77  ACU-FERIADO-AGEN            PIC  9(009) COMP-3  VALUE ZEROS.

070705
03     77  ACU-NF-DETALHES             PIC  9(009) COMP-3  VALUE ZEROS.
03     77  ACU-DETALHES-ENCONT         PIC  9(009) COMP-3  VALUE ZEROS.

071129 77  ACU-GRAV-ARQCOBR2           PIC  9(009) COMP-3  VALUE ZEROS.
071130 77  ACU-GRAV-ARQATU92           PIC  9(009) COMP-3  VALUE ZEROS.
071130 77  ACU-GRAV-ARQATUH7           PIC  9(009) COMP-3  VALUE ZEROS.

       77  ACU-COMMIT                  PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** TESTES DE FILE STATUS ***'.
      *----------------------------------------------------------------*

       77  WRK-FS-ATUALIZA             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-DETALHES             PIC  X(002)         VALUE SPACES.
070710
05     77  WRK-FS-ATLZB0H4             PIC  X(002)         VALUE SPACES.
070710
06     77  WRK-FS-ARQDEBIT             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-CTRLDATA             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SAIDETAL             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-FITCONTB             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-OCOR1182             PIC  X(002)         VALUE SPACES.

071129 77  WRK-FS-ARQCOBR2             PIC  X(002)         VALUE SPACES.
071130 77  WRK-FS-ARQATU92             PIC  X(002)         VALUE SPACES.
071130 77  WRK-FS-ARQATUH7             PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA  LEITURA '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-MASCARA                 PIC  ZZ.ZZZ.ZZZ.ZZ9 VALUE ZEROS.
       77  WRK-SQLCODE                 PIC  9(003)         VALUE ZEROS.
       77  WRK-RETURN-CODE             PIC  9(003)         VALUE ZEROS.
       77  WRK-DATA-AGENDAMENTO        PIC  X(010)         VALUE SPACES.
       77  WRK-VLR-DETALHES            PIC  9(011)V99      VALUE ZEROS.
       77  WRK-VLR-ATUALIZA            PIC  9(011)V99      VALUE ZEROS.
       77  WRK-VR-SALDO                PIC  9(011)V99      VALUE ZEROS.
       77  WRK-VR-SALDO-PK             PIC  9(011)V99 COMP-3 VALUE 0.
       77  WRK-COBR-VALOR-DEBITAR-PK   PIC  9(011)V99 COMP-3 VALUE 0.
       77  WRK-VLR-RESTANTE            PIC  9(011)V99      VALUE ZEROS.
       77  WRK-VLR-COBRADO-EVENTO      PIC  9(011)V99      VALUE ZEROS.
       77  WRK-CSIT-EVNTO-5525         PIC  9(002)         VALUE ZEROS.
070705
04     77  WRK-RETORNO-F               PIC  9(002)         VALUE ZEROS.
       77  WRK-GRAVA-CONTABIL          PIC  X(001)         VALUE SPACES.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA VARIAVEIS DE DATAS ***'.
      *----------------------------------------------------------------*
070705
04     01  WRK-DEFETV-DEB-MOVTO        PIC  X(010)         VALUE SPACES.

       01  WRK-DATA-AUX.
           05  WRK-ANO-AUX             PIC  9(004)         VALUE ZEROS.
           05  WRK-MES-AUX             PIC  9(002)         VALUE ZEROS.
           05  WRK-DIA-AUX             PIC  9(002)         VALUE ZEROS.

       01  WRK-DATA-DB2.
           05  WRK-DIA-DB2             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  WRK-MES-DB2             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  WRK-ANO-DB2             PIC  9(004)         VALUE ZEROS.
070710
08     01  WRK-DATA-EVT.
08         03  WRK-DIA-EVT             PIC  9(002)         VALUE ZEROS.
08         03  FILLER                  PIC  X(001)         VALUE '.'.
08         03  WRK-MES-EVT             PIC  9(002)         VALUE ZEROS.
08         03  FILLER                  PIC  X(001)         VALUE '.'.
08         03  WRK-ANO-EVT             PIC  9(004)         VALUE ZEROS.

070710
06     01  WRK-DATA-DEB.
06         03  WRK-DIA-DEB             PIC  9(002)         VALUE ZEROS.
06         03  FILLER                  PIC  X(001)         VALUE '.'.
06         03  WRK-MES-DEB             PIC  9(002)         VALUE ZEROS.
06         03  FILLER                  PIC  X(001)         VALUE '.'.
06         03  WRK-ANO-DEB             PIC  9(004)         VALUE ZEROS.
070710
05     01  WRK-DATA.
05         03  W-DD                    PIC 9(02) VALUE ZEROS.
05         03  FILLER                  PIC X(01) VALUE SPACES.
05         03  W-MM                    PIC 9(02) VALUE ZEROS.
05         03  FILLER                  PIC X(01) VALUE SPACES.
05         03  W-AAAA                  PIC 9(04) VALUE ZEROS.
05
05     01  WRK-TIMESTAMP-H4.
05         03  WRK-DTIME.
05             05  WRK-AAAA            PIC 9(04)    VALUE ZEROS.
05             05  WRK-TRACO1          PIC X(01)    VALUE SPACES.
05             05  WRK-MM              PIC 9(02)    VALUE ZEROS.
05             05  WRK-TRACO2          PIC X(01)    VALUE SPACES.
05             05  WRK-DD              PIC 9(02)    VALUE ZEROS.
05         03  WRK-RESTO.
05             05  WRK-TRACO3          PIC X(01)    VALUE SPACES.
05             05  WRK-HR              PIC 9(02)    VALUE ZEROS.
05             05  WRK-PONTO1          PIC X(01)    VALUE SPACES.
05             05  WRK-MI              PIC 9(02)    VALUE ZEROS.
05             05  WRK-PONTO2          PIC X(01)    VALUE SPACES.
05             05  WRK-SG              PIC 9(02)    VALUE ZEROS.
05             05  WRK-PONTO3          PIC X(01)    VALUE SPACES.
05             05  WRK-DS              PIC 9(06)    VALUE ZEROS.
05
05     01  WRK-TIMESTAMP-SIST.
05         05  WRK-AAAA-S              PIC 9(04)    VALUE ZEROS.
05         05  WRK-MM-S                PIC 9(02)    VALUE ZEROS.
05         05  WRK-DD-S                PIC 9(02)    VALUE ZEROS.
05         05  WRK-HR-S                PIC 9(02)    VALUE ZEROS.
05         05  WRK-MI-S                PIC 9(02)    VALUE ZEROS.
05         05  WRK-SG-S                PIC 9(02)    VALUE ZEROS.
05         05  WRK-DS-S                PIC 9(06)    VALUE ZEROS.
05
05     01  WRK-DT-R                    PIC 9(09)    VALUE ZEROS.
05     01  WRK-DATA-AAAAMMDD.
05         05  WRK-DATA-LIXO           PIC 9(01)    VALUE ZEROS.
05         05  WRK-DATA-AAAA           PIC 9(04)    VALUE ZEROS.
05         05  WRK-DATA-MM             PIC 9(02)    VALUE ZEROS.
05         05  WRK-DATA-DD             PIC 9(02)    VALUE ZEROS.
05
05    *---------------------------------------------------------------*
05     01  FILLER                      PIC  X(050)         VALUE
05         '*** AREA DA POOL7600 ***'.
05    *---------------------------------------------------------------*
05
05     01  WRK-DATA-HORA.
05         03  WRK-DT-JULIANA          PIC  9(005) COMP-3  VALUE ZEROS.
05         03  WRK-DT-AAMMDD           PIC  9(007) COMP-3  VALUE ZEROS.
05         03  WRK-DT-AAAAMMDD         PIC  9(009) COMP-3  VALUE ZEROS.
05         03  WRK-TI-HHMMSS           PIC  9(007) COMP-3  VALUE ZEROS.
05         03  WRK-TI-HHMMSSMMMMMM     PIC  9(013) COMP-3  VALUE ZEROS.
05         03  WRK-TIMESTAMP           PIC  X(020)         VALUE SPACES.


PCT    01 WRK-CPTCAO-GFCTB0H4          PIC  X(026) VALUE SPACES.
       01 FILLER                       REDEFINES WRK-CPTCAO-GFCTB0H4.
           05 FILLER                   PIC X(023).
PCT        05 WRK-CPTCAO-B0H4          PIC 9(003).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE MENSAGEM DE ERRO ***'.
      *----------------------------------------------------------------*

       01  WRK-MSG-FS.
           05  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           05  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           05  WRK-ARQUIVO             PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(017)         VALUE
               ' - FILE STATUS = '.
           05  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(003)         VALUE ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** COMUNICACAO COM MODULO GFCT5525 ***'.
      *----------------------------------------------------------------*

       01  WRK-5525-AREA-ENTRADA.
           05  WRK-5525-AMBIENTE       PIC  X(001)         VALUE SPACES.
           05  WRK-5525-CHAMADOR       PIC  X(008)         VALUE SPACES.
           05  WRK-5525-DENVIO-MOVTO   PIC  X(010)         VALUE SPACES.
           05  WRK-5525-CROTNA-ORIGE   PIC  X(004)         VALUE SPACES.
           05  WRK-5525-CNRO-ARQ       PIC  9(002)         VALUE ZEROS.
           05  WRK-5525-CSEQ-MOVTO     PIC  9(011)         VALUE ZEROS.
           05  WRK-5525-CSIT-EVNTO     PIC  9(002)         VALUE ZEROS.
           05  WRK-5525-CORRENTISTA    PIC  X(001)         VALUE SPACES.

       01  WRK-5525-AREA-SAIDA.
           05  WRK-5525-COD-RETORNO    PIC  9(002)         VALUE ZEROS.
           05  WRK-5525-COD-SQL-ERRO   PIC S9(003)         VALUE ZEROS.
           05  WRK-5525-DESC-MSG       PIC  X(070)         VALUE SPACES.

       COPY 'I#GFCT0M'.

       01  WRK-IO-PCB.
           05  WRK-IO-TERM             PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
           05  WRK-IO-STA              PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE SPACES.
           05  WRK-IO-MODNAME          PIC  X(008)         VALUE SPACES.

       01  WRK-ALT-PCB.
           05  WRK-ALT-PCBNAME         PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
           05  WRK-ALT-STATUS          PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE SPACES.
           05  WRK-ALT-MODNAME         PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA POOL7100 ***'.
      *----------------------------------------------------------------*

       COPY 'POL7100C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB092
           END-EXEC.

071130*    EXEC SQL
071130*        INCLUDE GFCTB0H7
071130*    END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** FIM DA WORKING STORAGE SECTION ***'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      *ROTINA PRINCIPAL DO PROGRAMA                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
070705             COBR-CHAVE-COBRANCA OF DETALHES EQUAL HIGH-VALUES
03                                     AND
03                 COBR-CHAVE-COBRANCA OF ATUALIZA EQUAL HIGH-VALUES.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA ABERTURA DOS ARQUIVOS                               *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           OPEN INPUT  ATUALIZA
                       DETALHES
                       CTRLDATA
                OUTPUT SAIDETAL
070710                 FITCONTB
05                     ATLZB0H4
11                     ARQDEBIT
                       OCOR1182
071129                 ARQCOBR2
071130                 ARQATU92
071130                 ARQATUH7.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1010-RESTART.
070710     PERFORM 1100-TESTAR-FILE-STATUS.
05         PERFORM 1200-FORMATAR-TIMESTAMP-H4.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1010-RESTART                    SECTION.
      *----------------------------------------------------------------*

      ***** VERIFICAR SE ROTINA DEVE SER INCLUIDA.
      ***** VAI PERDER MUITO TEMPO PARA CINCRONIZAR 2 TAB. E 5 ARQ.
      ***** EM CASO DE ABEND MELHOR RODAR NOVAMENTE O JOB.

      *----------------------------------------------------------------*
       1010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA TESTES DE FILE STATUS                               *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-ATUALIZA.

           PERFORM 1120-TESTAR-FS-DETALHES.

           PERFORM 1130-TESTAR-FS-CTRLDATA.

           PERFORM 1140-TESTAR-FS-SAIDETAL.

           PERFORM 1150-TESTAR-FS-FITCONTB.

           PERFORM 1160-TESTAR-FS-OCOR1182.
070710
05         PERFORM 1170-TESTAR-FS-ATLZB0H4.
070710
06         PERFORM 1180-TESTAR-FS-ARQDEBIT.

071129     PERFORM 1190-TESTAR-FS-ARQCOBR2.

071130     PERFORM 1192-TESTAR-FS-ARQATU92.

071130     PERFORM 1194-TESTAR-FS-ARQATUH7.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA TESTE DE FILE STATUS DO ARQUIVO ATUALIZA            *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-ATUALIZA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ATUALIZA         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-ATUALIZA    TO WRK-FILE-STATUS
               MOVE 'ATUALIZA '        TO WRK-ARQUIVO
               MOVE WRK-MSG-FS         TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA TESTE DE FILE STATUS DO ARQUIVO DETALHES            *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-DETALHES         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-DETALHES         NOT EQUAL '00'
               MOVE WRK-FS-DETALHES    TO WRK-FILE-STATUS
               MOVE 'DETALHES'         TO WRK-ARQUIVO
               MOVE WRK-MSG-FS         TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA TESTE DE FILE STATUS DO ARQUIVO CTRLDATA            *
      ******************************************************************
      *----------------------------------------------------------------*
       1130-TESTAR-FS-CTRLDATA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CTRLDATA         NOT EQUAL '00'
               MOVE WRK-FS-CTRLDATA    TO WRK-FILE-STATUS
               MOVE 'CTRLDATA'         TO WRK-ARQUIVO
               MOVE WRK-MSG-FS         TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA TESTE DE FILE STATUS DO ARQUIVO SAIDETAL            *
      ******************************************************************
      *----------------------------------------------------------------*
       1140-TESTAR-FS-SAIDETAL         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SAIDETAL         NOT EQUAL '00'
               MOVE WRK-FS-SAIDETAL    TO WRK-FILE-STATUS
               MOVE 'SAIDETAL'         TO WRK-ARQUIVO
               MOVE WRK-MSG-FS         TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA TESTE DE FILE STATUS DO ARQUIVO FITCONTB            *
      ******************************************************************
      *----------------------------------------------------------------*
       1150-TESTAR-FS-FITCONTB         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-FITCONTB         NOT EQUAL '00'
               MOVE WRK-FS-FITCONTB    TO WRK-FILE-STATUS
               MOVE 'FITCONTB'         TO WRK-ARQUIVO
               MOVE WRK-MSG-FS         TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       115000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA TESTE DE FILE STATUS DO ARQUIVO OCOR1182            *
      ******************************************************************
      *----------------------------------------------------------------*
       1160-TESTAR-FS-OCOR1182         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-OCOR1182         NOT EQUAL '00'
               MOVE WRK-FS-OCOR1182    TO WRK-FILE-STATUS
               MOVE 'OCOR1182'         TO WRK-ARQUIVO
               MOVE WRK-MSG-FS         TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1160-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
070710
05    ******************************************************************
05    *ROTINA PARA TESTE DE FILE STATUS DO ARQUIVO ATLZB0H4            *
05    ******************************************************************
05    *----------------------------------------------------------------*
05     1170-TESTAR-FS-ATLZB0H4         SECTION.
05    *----------------------------------------------------------------*
05
05         IF  WRK-FS-ATLZB0H4         NOT EQUAL '00'
05             MOVE WRK-FS-ATLZB0H4    TO WRK-FILE-STATUS
05             MOVE 'OCOR1182'         TO WRK-ARQUIVO
05             MOVE WRK-MSG-FS         TO ERR-TEXTO
05             MOVE 'APL'              TO ERR-TIPO-ACESSO
05             PERFORM 9999-PROCESSAR-ROTINA-ERRO
05         END-IF.
05
05    *----------------------------------------------------------------*
05     1170-99-FIM.                    EXIT.
05    *----------------------------------------------------------------*
070710
06    ******************************************************************
06    * TESTAR FILE STATUS DO ARQUIVO ARQDEBIT                         *
06    ******************************************************************
06    *----------------------------------------------------------------*
06     1180-TESTAR-FS-ARQDEBIT         SECTION.
06    *----------------------------------------------------------------*
06
06         IF  WRK-FS-ARQDEBIT         NOT EQUAL '00'
06             MOVE WRK-FS-ARQDEBIT    TO WRK-FILE-STATUS
06             MOVE 'ARQDEBIT'         TO WRK-ARQUIVO
06             MOVE WRK-MSG-FS         TO ERR-TEXTO
06             MOVE 'APL'              TO ERR-TIPO-ACESSO
06             PERFORM 9999-PROCESSAR-ROTINA-ERRO
06         END-IF.
06
06    *----------------------------------------------------------------*
06     1180-99-FIM.                    EXIT.
06    *----------------------------------------------------------------*
070710

071129******************************************************************
071129* TESTAR FILE STATUS DO ARQUIVO ARQCOBR2                         *
071129******************************************************************
071129*----------------------------------------------------------------*
071129 1190-TESTAR-FS-ARQCOBR2         SECTION.
071129*----------------------------------------------------------------*
071129
071129     IF  WRK-FS-ARQCOBR2         NOT EQUAL '00'
071129         MOVE WRK-FS-ARQCOBR2    TO WRK-FILE-STATUS
071129         MOVE 'ARQCOBR2'         TO WRK-ARQUIVO
071129         MOVE WRK-MSG-FS         TO ERR-TEXTO
071129         MOVE 'APL'              TO ERR-TIPO-ACESSO
071129         PERFORM 9999-PROCESSAR-ROTINA-ERRO
071129     END-IF.
071129
071129*----------------------------------------------------------------*
071129 1190-99-FIM.                    EXIT.
071129*----------------------------------------------------------------*

071130******************************************************************
071130* TESTAR FILE STATUS DO ARQUIVO ARQATU92                         *
071130******************************************************************
071130*----------------------------------------------------------------*
071130 1192-TESTAR-FS-ARQATU92         SECTION.
071130*----------------------------------------------------------------*
071130
071130     IF  WRK-FS-ARQATU92         NOT EQUAL '00'
071130         MOVE WRK-FS-ARQATU92    TO WRK-FILE-STATUS
071130         MOVE 'ARQATU92'         TO WRK-ARQUIVO
071130         MOVE WRK-MSG-FS         TO ERR-TEXTO
071130         MOVE 'APL'              TO ERR-TIPO-ACESSO
071130         PERFORM 9999-PROCESSAR-ROTINA-ERRO
071130     END-IF.
071130
071130*----------------------------------------------------------------*
071130 1192-99-FIM.                    EXIT.
071130*----------------------------------------------------------------*

071130******************************************************************
071130* TESTAR FILE STATUS DO ARQUIVO ARQATUH7                         *
071130******************************************************************
071130*----------------------------------------------------------------*
071130 1194-TESTAR-FS-ARQATUH7         SECTION.
071130*----------------------------------------------------------------*
071130
071130     IF  WRK-FS-ARQATUH7         NOT EQUAL '00'
071130         MOVE WRK-FS-ARQATUH7    TO WRK-FILE-STATUS
071130         MOVE 'ARQATUH7'         TO WRK-ARQUIVO
071130         MOVE WRK-MSG-FS         TO ERR-TEXTO
071130         MOVE 'APL'              TO ERR-TIPO-ACESSO
071130         PERFORM 9999-PROCESSAR-ROTINA-ERRO
071130     END-IF.
071130
071130*----------------------------------------------------------------*
071130 1194-99-FIM.                    EXIT.
071130*----------------------------------------------------------------*

05    ******************************************************************
05    * FORMATAR TIMESTAMP                                             *
05    ******************************************************************
05    *----------------------------------------------------------------*
05     1200-FORMATAR-TIMESTAMP-H4     SECTION.
05    *----------------------------------------------------------------*
05
05         CALL 'POOL7600'             USING WRK-DATA-HORA.
05
05         MOVE WRK-DT-AAAAMMDD        TO WRK-DT-R.
05         MOVE WRK-DT-R               TO WRK-DATA-AAAAMMDD.
05         MOVE WRK-DATA-DD            TO WRK-DD.
05         MOVE WRK-DATA-MM            TO WRK-MM.
05         MOVE WRK-DATA-AAAA          TO WRK-AAAA.
05         MOVE '-'                    TO WRK-TRACO1 WRK-TRACO2.
05
05         MOVE  WRK-TIMESTAMP         TO WRK-TIMESTAMP-SIST.
05         MOVE  WRK-HR-S              TO WRK-HR.
05         MOVE  WRK-MI-S              TO WRK-MI.
05         MOVE  WRK-SG-S              TO WRK-SG.
05         MOVE  WRK-DS-S              TO WRK-DS.
05         MOVE  '-'                   TO WRK-TRACO3.
05         MOVE  '.'                   TO WRK-PONTO1
05                                        WRK-PONTO2
05                                        WRK-PONTO3.
05
05    *----------------------------------------------------------------*
05     1200-99-FIM.                    EXIT.
05    *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA VERIFICAR VAZIO                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-ATUALIZA.

070705
03    *    IF  COBR-CHAVE-COBRANCA     OF ATUALIZA
03    *                                EQUAL HIGH-VALUES
03    *        DISPLAY '***************** GFCT1182 *****************'
03    *        DISPLAY '*                                          *'
03    *        DISPLAY '*          ARQUIVO ATUALIZA VAZIO          *'
03    *        DISPLAY '*         PROCESSAMENTO  ENCERRADO         *'
03    *        DISPLAY '*                                          *'
03    *        DISPLAY '***************** GFCT1182 *****************'
03    *        PERFORM 4000-FINALIZAR
03    *    END-IF.

           PERFORM 2200-LER-DETALHES.

           IF  COBR-CHAVE-COBRANCA     OF DETALHES
                                       EQUAL HIGH-VALUES
               DISPLAY '***************** GFCT1182 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*          ARQUIVO DETALHES VAZIO          *'
               DISPLAY '*         PROCESSAMENTO  ENCERRADO         *'
               DISPLAY '*                                          *'
               DISPLAY '***************** GFCT1182 *****************'
               PERFORM 4000-FINALIZAR
           END-IF.

           PERFORM 2300-LER-CTRLDATA.

           IF  WRK-FS-CTRLDATA         EQUAL '10'
               DISPLAY '***************** GFCT1182 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*          ARQUIVO CTRLDATA VAZIO          *'
               DISPLAY '*         PROCESSAMENTO  ENCERRADO         *'
               DISPLAY '*                                          *'
               DISPLAY '***************** GFCT1182 *****************'
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA LEITURA DO ARQUIVO ATUALIZA                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-ATUALIZA               SECTION.
      *----------------------------------------------------------------*

           READ ATUALIZA.

           IF  WRK-FS-ATUALIZA        EQUAL '10'
               MOVE HIGH-VALUES       TO COBR-CHAVE-COBRANCA OF ATUALIZA
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-ATUALIZA.

           ADD 1                       TO ACU-LIDOS-ATUALIZA.

070705
01         IF  COBR-COD-RETORNO        OF ATUALIZA
01                                  EQUAL '001' OR '012'
01             MOVE ZEROS              TO
01                                     COBR-VALOR-DEBITADO OF ATUALIZA
01                                     WRK-VLR-ATUALIZA
                                       WRK-VR-SALDO
01         ELSE
01              MOVE COBR-VALOR-DEBITADO  OF ATUALIZA
01                                     TO WRK-VLR-ATUALIZA
                                          WRK-VR-SALDO
01         END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA LEITURA DO ARQUIVO DETALHES                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-LER-DETALHES               SECTION.
      *----------------------------------------------------------------*

           READ DETALHES.

           IF  WRK-FS-DETALHES        EQUAL '10'
               MOVE HIGH-VALUES       TO COBR-CHAVE-COBRANCA OF DETALHES
               GO TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-DETALHES.

           ADD 1                       TO ACU-LIDOS-DETALHES.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA LEITURA DO ARQUIVO CTRLDATA                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-LER-CTRLDATA               SECTION.
      *----------------------------------------------------------------*

           READ CTRLDATA.

           IF  WRK-FS-CTRLDATA         EQUAL '10'
               GO TO 2300-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1130-TESTAR-FS-CTRLDATA.

           ADD 1                       TO ACU-LIDOS-CTRLDATA.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA DE PROCESSAMENTO                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           IF  COBR-CHAVE-COBRANCA OF DETALHES LESS COBR-CHAVE-COBRANCA
                                               OF ATUALIZA
               MOVE REG-COBRANCA   OF DETALHES TO REG-COBRANCA
                                               OF SAIDETAL
               WRITE REG-COBRANCA              OF SAIDETAL
               PERFORM 1140-TESTAR-FS-SAIDETAL
               ADD 1                           TO ACU-GRAV-SAIDETAL
               PERFORM 2200-LER-DETALHES
           ELSE
               IF  COBR-CHAVE-COBRANCA         OF DETALHES
                                               EQUAL COBR-CHAVE-COBRANCA
                                               OF ATUALIZA
                   PERFORM 3100-TRATAR-IGUAL
                   PERFORM 2200-LER-DETALHES
070705
03                 ADD   1                     TO ACU-DETALHES-ENCONT
               ELSE
03                 IF  ACU-DETALHES-ENCONT      = ZEROS
03                     ADD 1                   TO ACU-NF-DETALHES
03                     IF  ACU-NF-DETALHES      < 1001
03                         DISPLAY 'REGISTRO CONSOLIDADO SEM DETALHES ='
03                         COBR-CHAVE-COBRANCA OF ATUALIZA
03                     END-IF
03                 END-IF
                   PERFORM 2100-LER-ATUALIZA
03                 MOVE  0                     TO ACU-DETALHES-ENCONT
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA TRATAR CHAVES IGUAIS                                *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-TRATAR-IGUAL               SECTION.
      *----------------------------------------------------------------*

           PERFORM 3110-ACESSAR-GFCTB092.

WS    *    DISPLAY '************************* NOVO REGISTRO GFCTB092'
WS    *    DISPLAY 'COBR-CHAVE-ORIGEM=' COBR-CHAVE-ORIGEM OF DETALHES.

           IF  SQLCODE                    NOT EQUAL ZEROS OR
               CSIT-EVNTO-RECBD           OF GFCTB092
                                          NOT EQUAL 13
               ADD 1                      TO ACU-DESPREZADOS
WS    *      DISPLAY 'DESPREZ=' SQLCODE ' ' CSIT-EVNTO-RECBD OF GFCTB092
               GO TO 3100-99-FIM
           END-IF.

WS    *    DISPLAY 'INICIO DO PROCESSO  '
WS    *    DISPLAY 'MORA/POUP- COD=' COBR-COD-RETORNO OF ATUALIZA
WS    *    DISPLAY 'VALOR DEBITAR =' WRK-VLR-ATUALIZA
WS    *    DISPLAY 'VALOR EVENTO  =' COBR-VALOR-DEBITAR OF DETALHES
WS    *    DISPLAY ' ******* GFCTB092 ******* '
WS    *    DISPLAY 'CROTNA_ORIGE_MOVTO=' CROTNA-ORIGE-MOVTO OF GFCTB092
WS    *    DISPLAY 'CNRO_ARQ_MOVTO    =' CNRO-ARQ-MOVTO OF GFCTB092
WS    *    DISPLAY 'CSEQ_MOVTO        =' CSEQ-MOVTO OF GFCTB092
WS    *    DISPLAY 'DENVIO_MOVTO_TARIF=' DENVIO-MOVTO-TARIF OF GFCTB092
WS    *    DISPLAY 'CROTNA_ORIGE_MOVTO=' CROTNA-ORIGE-MOVTO OF GFCTB092
WS    *    DISPLAY 'CNRO_ARQ_MOVTO    =' CNRO-ARQ-MOVTO OF GFCTB092
WS    *    DISPLAY 'CSEQ_MOVTO        =' CSEQ-MOVTO OF GFCTB092
WS    *    DISPLAY 'VTARIF_LIQ_MOVTO  =' VTARIF-LIQ-MOVTO OF GFCTB092
WS    *    DISPLAY 'VTARIF_DEB_MOVTO  =' VTARIF-DEB-MOVTO OF GFCTB092
WS    *    DISPLAY 'DAGNDA_DEB_MOVTO  =' DAGNDA-DEB-MOVTO OF GFCTB092
WS    *    DISPLAY 'DEFETV_DEB_MOVTO  =' DEFETV-DEB-MOVTO OF GFCTB092
WS    *    DISPLAY 'QMAX_TENTV_DEB    =' QMAX-TENTV-DEB OF GFCTB092
WS    *    DISPLAY 'QTENTV_DEB_MOVTO  =' QTENTV-DEB-MOVTO OF GFCTB092
WS    *    DISPLAY 'CSIT_EVNTO_RECBD  =' CSIT-EVNTO-RECBD OF GFCTB092.

           IF  COBR-COD-RETORNO           OF ATUALIZA
070705                                    EQUAL ('000' OR '010' OR
01                                               '001' OR '012' OR
                                                 'P'   OR 'I')
           IF  WRK-VLR-ATUALIZA       NOT LESS
                                          COBR-VALOR-DEBITAR OF DETALHES
WS    *            DISPLAY 'COBRAR  TOTAL'
WS                 MOVE COBR-VALOR-DEBITAR OF DETALHES
WS                                         TO  WRK-VLR-COBRADO-EVENTO
                   PERFORM 3120-EVENTO-COBRADO-TOTAL
               ELSE
WS    *            DISPLAY 'COBRAR  PARCIAL'
WS                 MOVE WRK-VLR-ATUALIZA   TO  WRK-VLR-COBRADO-EVENTO
                   PERFORM 3130-EVENTO-COBRADO-PARCIAL
               END-IF
           ELSE
______*________IF__COBR-COD-RETORNO_______OF_ATUALIZA
______*___________________________________EQUAL_'009'
WS____*____________DISPLAY_'DESPREZAR__-_FERIADO_MORA'
______*____________ADD_1__________________TO_ACU-DESPREZADOS
______*________ELSE
                   IF  COBR-COD-RETORNO   OF ATUALIZA
                                          EQUAL 'F' OR '009'
                       PERFORM 3140-RETORNAR-SIT-EVENTO
                   ELSE

VINI                   MOVE COBR-RETORNO OF ATUALIZA
VINI                                         TO COBR-RETORNO OF DETALHES
VINI                   MOVE REG-COBRANCA  OF DETALHES
VINI                                      TO REG-COBRANCA    OF OCOR1182
WS    *                DISPLAY 'OCORRENCIA-RETORNO ATUALIZA NAO PREVIST'
070705
02                     MOVE 14       TO COBR-SITUACAO-EVENTO OF OCOR1182
                       MOVE 'GFCT0518'    TO COBR-JOB        OF OCOR1182
                       MOVE 'GFCT1182'    TO COBR-PROGRAMA   OF OCOR1182
                       WRITE REG-COBRANCA OF OCOR1182
                       PERFORM 1160-TESTAR-FS-OCOR1182
                       ADD 1              TO ACU-GRAV-OCOR1182
                   END-IF
      *        END-IF
           END-IF.

WS    *    DISPLAY 'FINAL  DO PROCESSO  '
WS    *    DISPLAY 'NOVO VALOR DEBITAR =' WRK-VLR-ATUALIZA
WS    *    DISPLAY ' ******* GFCTB092 ******* '
WS    *    DISPLAY 'CROTNA_ORIGE_MOVTO=' CROTNA-ORIGE-MOVTO OF GFCTB092
WS    *    DISPLAY 'CNRO_ARQ_MOVTO    =' CNRO-ARQ-MOVTO OF GFCTB092
WS    *    DISPLAY 'CSEQ_MOVTO        =' CSEQ-MOVTO OF GFCTB092
WS    *    DISPLAY 'DENVIO_MOVTO_TARIF=' DENVIO-MOVTO-TARIF OF GFCTB092
WS    *    DISPLAY 'CROTNA_ORIGE_MOVTO=' CROTNA-ORIGE-MOVTO OF GFCTB092
WS    *    DISPLAY 'CNRO_ARQ_MOVTO    =' CNRO-ARQ-MOVTO OF GFCTB092
WS    *    DISPLAY 'CSEQ_MOVTO        =' CSEQ-MOVTO OF GFCTB092
WS    *    DISPLAY 'VTARIF_LIQ_MOVTO  =' VTARIF-LIQ-MOVTO OF GFCTB092
WS    *    DISPLAY 'VTARIF_DEB_MOVTO  =' VTARIF-DEB-MOVTO OF GFCTB092
WS    *    DISPLAY 'DAGNDA_DEB_MOVTO  =' DAGNDA-DEB-MOVTO OF GFCTB092
WS    *    DISPLAY 'DEFETV_DEB_MOVTO  =' DEFETV-DEB-MOVTO OF GFCTB092
WS    *    DISPLAY 'QMAX_TENTV_DEB    =' QMAX-TENTV-DEB OF GFCTB092
WS    *    DISPLAY 'QTENTV_DEB_MOVTO  =' QTENTV-DEB-MOVTO OF GFCTB092.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA ACESSAR TABELA GFCTB092                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3110-ACESSAR-GFCTB092           SECTION.
      *----------------------------------------------------------------*
070705
04         MOVE ZEROS                  TO WRK-RETORNO-F.

           MOVE COBR-DATA-ORIG         OF DETALHES
                                       TO DENVIO-MOVTO-TARIF
                                                           OF GFCTB092.
           MOVE COBR-ROTINA-ORIG       OF DETALHES
                                       TO CROTNA-ORIGE-MOVTO
                                                           OF GFCTB092.
           MOVE COBR-NUM-ARQ-ORIG      OF DETALHES
                                       TO CNRO-ARQ-MOVTO   OF GFCTB092.
           MOVE COBR-SEQ-ARQ-ORIG      OF DETALHES
                                       TO CSEQ-MOVTO       OF GFCTB092.

           EXEC SQL
             SELECT
                   DENVIO_MOVTO_TARIF,
                   CROTNA_ORIGE_MOVTO,
                   CNRO_ARQ_MOVTO,
                   CSEQ_MOVTO,
                   CSERVC_TARIF,
                   CMIDIA_SERVC_MOVTO,
                   CPERIF_SERVC_MOVTO,
                   QEVNTO_AGRUP,
                   HPREST_SERVC_MOVTO,
                   CTPO_CTA_MOVTO,
                   CBCO_DSTNO_MOVTO,
                   CAG_DSTNO_MOVTO,
                   CCTA_DSTNO_MOVTO,
                   CPAB_DSTNO_MOVTO,
                   CAG_CTLZA_DEB,
                   CCTA_CTLZA_DEB,
                   VTARIF_BRUTO_MOVTO,
                   VTARIF_LIQ_MOVTO,
                   VTARIF_DEB_MOVTO,
                   DAGNDA_DEB_MOVTO,
                   DEFETV_DEB_MOVTO,
                   QMAX_TENTV_DEB,
                   QTENTV_DEB_MOVTO,
                   CSIT_EVNTO_RECBD,
                   CIDTFD_PGMC_COBR,
                   DOCOR_EVNTO,
                   VTARIF_CADTR_SIST
             INTO
                   :GFCTB092.DENVIO-MOVTO-TARIF,
                   :GFCTB092.CROTNA-ORIGE-MOVTO,
                   :GFCTB092.CNRO-ARQ-MOVTO,
                   :GFCTB092.CSEQ-MOVTO,
                   :GFCTB092.CSERVC-TARIF,
                   :GFCTB092.CMIDIA-SERVC-MOVTO,
                   :GFCTB092.CPERIF-SERVC-MOVTO,
                   :GFCTB092.QEVNTO-AGRUP,
                   :GFCTB092.HPREST-SERVC-MOVTO,
                   :GFCTB092.CTPO-CTA-MOVTO,
                   :GFCTB092.CBCO-DSTNO-MOVTO,
                   :GFCTB092.CAG-DSTNO-MOVTO,
                   :GFCTB092.CCTA-DSTNO-MOVTO,
                   :GFCTB092.CPAB-DSTNO-MOVTO,
                   :GFCTB092.CAG-CTLZA-DEB,
                   :GFCTB092.CCTA-CTLZA-DEB,
                   :GFCTB092.VTARIF-BRUTO-MOVTO,
                   :GFCTB092.VTARIF-LIQ-MOVTO,
                   :GFCTB092.VTARIF-DEB-MOVTO,
                   :GFCTB092.DAGNDA-DEB-MOVTO,
                   :GFCTB092.DEFETV-DEB-MOVTO,
                   :GFCTB092.QMAX-TENTV-DEB,
                   :GFCTB092.QTENTV-DEB-MOVTO,
                   :GFCTB092.CSIT-EVNTO-RECBD,
                   :GFCTB092.CIDTFD-PGMC-COBR,
                   :GFCTB092.DOCOR-EVNTO,
                   :GFCTB092.VTARIF-CADTR-SIST
             FROM   DB2PRD.MOVTO_EVNTO_CRRTT
             WHERE
                   DENVIO_MOVTO_TARIF = :GFCTB092.DENVIO-MOVTO-TARIF AND
                   CROTNA_ORIGE_MOVTO = :GFCTB092.CROTNA-ORIGE-MOVTO AND
                   CNRO_ARQ_MOVTO     = :GFCTB092.CNRO-ARQ-MOVTO     AND
                   CSEQ_MOVTO         = :GFCTB092.CSEQ-MOVTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
VINI           MOVE COBR-RETORNO OF ATUALIZA
VINI                                   TO COBR-RETORNO OF DETALHES
VINI           MOVE REG-COBRANCA  OF DETALHES
VINI                                   TO REG-COBRANCA    OF OCOR1182
               MOVE 'GFCT0518'         TO COBR-JOB         OF OCOR1182
               MOVE 'GFCT1182'         TO COBR-PROGRAMA    OF OCOR1182
070705
02             MOVE 14             TO COBR-SITUACAO-EVENTO OF OCOR1182
               MOVE 'GFCTB092'         TO COBR-RECURSO     OF OCOR1182
               MOVE SQLCODE            TO WRK-SQLCODE
               MOVE WRK-SQLCODE        TO COBR-COD-RETORNO OF OCOR1182
               MOVE 'ERRO NO SELECT DA TABELA'
                                       TO COBR-DESCRICAO   OF OCOR1182
               WRITE REG-COBRANCA      OF OCOR1182
               PERFORM 1160-TESTAR-FS-OCOR1182
               ADD 1                   TO ACU-GRAV-OCOR1182
           ELSE
               MOVE DEFETV-DEB-MOVTO   OF GFCTB092
070705                                 TO WRK-DATA-DEB
04                                        WRK-DEFETV-DEB-MOVTO
           MOVE DAGNDA-DEB-MOVTO   OF GFCTB092
                                       TO WRK-DATA-AGENDAMENTO
               MOVE VTARIF-DEB-MOVTO   OF GFCTB092
                                       TO WRK-VLR-DETALHES
           END-IF.

      *    DISPLAY 'SELECT GFCTB092' SQLCODE.
      *----------------------------------------------------------------*
       3110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA TRATAR EVENTO COBRADO TOTAL                         *
      ******************************************************************
      *----------------------------------------------------------------*
       3120-EVENTO-COBRADO-TOTAL       SECTION.
      *----------------------------------------------------------------*
           COMPUTE WRK-VLR-DETALHES    = COBR-VALOR-DEBITAR OF DETALHES
                                       + VTARIF-DEB-MOVTO   OF GFCTB092.
           SUBTRACT COBR-VALOR-DEBITAR OF DETALHES
                                       FROM WRK-VLR-ATUALIZA.
           MOVE 5                      TO WRK-CSIT-EVNTO-5525
           MOVE GFCT06-DPROCM-ANTER    TO WRK-DATA-DEB.

           PERFORM 3121-ATUALIZA-GFCTB092.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 3123-ATUALIZAR-SIT-EVENTO
070710         PERFORM 3124-GRAVAR-FITA4
10             IF COBR-VALOR-DEBITAR   OF DETALHES
10                                 NOT EQUAL
10                VTARIF-DEB-MOVTO     OF GFCTB092
10                PERFORM 3133-GRAVAR-COB-PARCIAL
10             END-IF
           END-IF.

      *----------------------------------------------------------------*
       3120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA ATUALIZACAO DA TABELA GFCTB092                      *
      ******************************************************************
      *----------------------------------------------------------------*
       3121-ATUALIZA-GFCTB092          SECTION.
      *----------------------------------------------------------------*

070705
04    *---> ATUALIZA SITUACAO DO EVENTO
04         IF   WRK-RETORNO-F        = ZEROS
                MOVE WRK-CSIT-EVNTO-5525
04                                  TO CSIT-EVNTO-RECBD OF GFCTB092
04         ELSE
                MOVE WRK-RETORNO-F
04                                  TO CSIT-EVNTO-RECBD OF GFCTB092
04         END-IF.

WS    *---> ATUALIZA AGENCIA/CONTA NO ARQUIVO ATUALIZA
           MOVE COBR-TRAG-EXT       OF ATUALIZA
                                    TO COBR-TRAG-EXT    OF DETALHES.
           MOVE COBR-AGENCIA-RET    OF DETALHES
                                    TO COBR-AGENCIA-DEB OF DETALHES.
           MOVE COBR-CONTA-RET      OF DETALHES
                                    TO COBR-CONTA-DEB   OF DETALHES.

WS    *---> ATUALIZA AGENCIA/CONTA NA TABELA GFCTB092
           IF COBR-CENTRALIZ        OF DETALHES   = 'S'
              MOVE COBR-AGENCIA-RET OF DETALHES  TO  CAG-CTLZA-DEB
              MOVE COBR-CONTA-RET   OF DETALHES  TO  CCTA-CTLZA-DEB
           ELSE
              MOVE COBR-AGENCIA-RET OF DETALHES  TO  CAG-DSTNO-MOVTO
              MOVE COBR-CONTA-RET   OF DETALHES  TO  CCTA-DSTNO-MOVTO
           END-IF.

           MOVE WRK-DATA-DEB          TO DEFETV-DEB-MOVTO   OF GFCTB092.
           MOVE WRK-DATA-AGENDAMENTO  TO DAGNDA-DEB-MOVTO   OF GFCTB092.
           MOVE WRK-VLR-DETALHES      TO VTARIF-DEB-MOVTO   OF GFCTB092.
           MOVE COBR-DATA-ORIG        OF DETALHES
                                      TO DENVIO-MOVTO-TARIF OF GFCTB092.
           MOVE COBR-ROTINA-ORIG      OF DETALHES
                                      TO CROTNA-ORIGE-MOVTO OF GFCTB092.
           MOVE COBR-NUM-ARQ-ORIG     OF DETALHES
                                      TO CNRO-ARQ-MOVTO     OF GFCTB092.
           MOVE COBR-SEQ-ARQ-ORIG     OF DETALHES
                                      TO CSEQ-MOVTO         OF GFCTB092.

071130***** INICIO ****** UPDATE SUBSTITUIDO POR ARQUIVO *************
      *    EXEC SQL
      *      UPDATE   DB2PRD.MOVTO_EVNTO_CRRTT
      *        SET
WS    *            CAG_DSTNO_MOVTO    = :GFCTB092.CAG-DSTNO-MOVTO ,
WS    *            CCTA_DSTNO_MOVTO   = :GFCTB092.CCTA-DSTNO-MOVTO,
WS    *            CAG_CTLZA_DEB      = :GFCTB092.CAG-CTLZA-DEB   ,
WS    *            CCTA_CTLZA_DEB     = :GFCTB092.CCTA-CTLZA-DEB  ,
      *            DEFETV_DEB_MOVTO   = :GFCTB092.DEFETV-DEB-MOVTO,
      *            DAGNDA_DEB_MOVTO   = :GFCTB092.DAGNDA-DEB-MOVTO,
      *            VTARIF_DEB_MOVTO   = :GFCTB092.VTARIF-DEB-MOVTO,
070710*            QTENTV_DEB_MOVTO   = :GFCTB092.QTENTV-DEB-MOVTO,
05*04 *            CSIT_EVNTO_RECBD   = :GFCTB092.CSIT-EVNTO-RECBD
      *      WHERE
      *            DENVIO_MOVTO_TARIF = :GFCTB092.DENVIO-MOVTO-TARIF AND
      *            CROTNA_ORIGE_MOVTO = :GFCTB092.CROTNA-ORIGE-MOVTO AND
      *            CNRO_ARQ_MOVTO     = :GFCTB092.CNRO-ARQ-MOVTO     AND
      *            CSEQ_MOVTO         = :GFCTB092.CSEQ-MOVTO
      *    END-EXEC.

      *    IF (SQLCODE                 NOT EQUAL ZEROS) OR
      *       (SQLWARN0                EQUAL 'W'      )
VINI  *        MOVE COBR-RETORNO OF ATUALIZA
VINI  *                                TO COBR-RETORNO     OF DETALHES
VINI  *        MOVE REG-COBRANCA  OF DETALHES
VINI  *                                TO REG-COBRANCA     OF OCOR1182
      *        MOVE 'GFCT0518'         TO COBR-JOB         OF OCOR1182
070705*
02    *        MOVE 14             TO COBR-SITUACAO-EVENTO OF OCOR1182
      *        MOVE 'GFCT1182'         TO COBR-PROGRAMA    OF OCOR1182
      *        MOVE 'GFCTB092'         TO COBR-RECURSO     OF OCOR1182
      *        MOVE SQLCODE            TO WRK-SQLCODE
      *        MOVE WRK-SQLCODE        TO COBR-COD-RETORNO OF OCOR1182
      *        MOVE 'ERRO NO UPDATE DA TABELA'
      *                                TO COBR-DESCRICAO   OF OCOR1182
      *        WRITE REG-COBRANCA      OF OCOR1182
      *        PERFORM 1160-TESTAR-FS-OCOR1182
      *        ADD 1                   TO ACU-GRAV-OCOR1182
071130*    ELSE
071130*        ADD 1                   TO ACU-COMMIT
071130*        IF  ACU-COMMIT          EQUAL 5000000
071130*            PERFORM 3122-EFETUAR-COMMIT
071130*            MOVE ZEROS          TO ACU-COMMIT
071130*        END-IF
      *    END-IF.

071130     MOVE DENVIO-MOVTO-TARIF TO GFCT67-DENVIO-MOVTO-TARIF
071130     MOVE CROTNA-ORIGE-MOVTO TO GFCT67-CROTNA-ORIGE-MOVTO
071130     MOVE CNRO-ARQ-MOVTO     TO GFCT67-CNRO-ARQ-MOVTO
071130     MOVE CSEQ-MOVTO         TO GFCT67-CSEQ-MOVTO
071130     MOVE CAG-DSTNO-MOVTO    TO GFCT67-CAG-DSTNO-MOVTO
071130     MOVE CCTA-DSTNO-MOVTO   TO GFCT67-CCTA-DSTNO-MOVTO
071130     MOVE CAG-CTLZA-DEB      TO GFCT67-CAG-CTLZA-DEB
071130     MOVE CCTA-CTLZA-DEB     TO GFCT67-CCTA-CTLZA-DEB
071130     MOVE QTENTV-DEB-MOVTO   TO GFCT67-QTENTV-DEB-MOVTO
071130     MOVE DAGNDA-DEB-MOVTO   TO GFCT67-DAGNDA-DEB-MOVTO
071130     MOVE VTARIF-DEB-MOVTO   TO GFCT67-VTARIF-DEB-MOVTO
071130     MOVE DEFETV-DEB-MOVTO   TO GFCT67-DEFETV-DEB-MOVTO
071130     MOVE CSIT-EVNTO-RECBD   TO GFCT67-CSIT-EVNTO-RECBD.

BI1210     MOVE COBR-NUM-PARC      OF DETALHES
BI1210                             TO GFCT67-NUM-PARC.

071130     WRITE GFCT67-GFCTB092.
071130
071130     PERFORM 1192-TESTAR-FS-ARQATU92.
071130
071130     ADD 1                       TO ACU-GRAV-ARQATU92.

071130**** FIM ********** UPDATE SUBSTITUIDO POR ARQUIVO *************

070929     PERFORM 3212-GRAVAR-ARQCOBR2.

      *----------------------------------------------------------------*
       3121-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

071130******************************************************************
071130*ROTINA PARA EFETUAR COMMIT                                      *
071130******************************************************************
071130*----------------------------------------------------------------*
071130*3122-EFETUAR-COMMIT             SECTION.
071130*----------------------------------------------------------------*
071130*
071130*    EXEC SQL
071130*        COMMIT
071130*    END-EXEC.
071130*
071130*    IF  SQLCODE                 NOT EQUAL ZEROS
071130*        MOVE 'DB2'              TO ERR-TIPO-ACESSO
071130*        MOVE 'COMMIT'           TO ERR-DBD-TAB
071130*                                   ERR-FUN-COMANDO
071130*        MOVE  SQLCODE           TO ERR-SQL-CODE
071130*        MOVE '0010'             TO ERR-LOCAL
071130*        MOVE  SPACES            TO ERR-SEGM
071130*        PERFORM 9999-PROCESSAR-ROTINA-ERRO
071130*    END-IF.
071130*
071130*----------------------------------------------------------------*
071130*3122-99-FIM.                    EXIT.
071130*----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA ATUALIZAR SITUACAO EVENTO                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3123-ATUALIZAR-SIT-EVENTO       SECTION.
      *----------------------------------------------------------------*
070710
05         MOVE DENVIO-MOVTO-TARIF     OF GFCTB092
05                                     TO ZD-DENVIO-MOVTO-TARIF.
05         MOVE CROTNA-ORIGE-MOVTO     OF GFCTB092
05                                     TO ZD-CROTNA-ORIGE-MOVTO.
05         MOVE CNRO-ARQ-MOVTO         OF GFCTB092
05                                     TO ZD-CNRO-ARQ-MOVTO.
05         MOVE CSEQ-MOVTO             OF GFCTB092
05                                     TO ZD-CSEQ-MOVTO.
05         MOVE WRK-TIMESTAMP-H4       TO ZD-HTROCA-SIT-EVNTO.
05         MOVE WRK-CSIT-EVNTO-5525    TO ZD-CSIT-EVNTO-RECBD.

           MOVE WRK-TIMESTAMP-H4       TO WRK-CPTCAO-GFCTB0H4
           MOVE WRK-CPTCAO-B0H4        TO ZD-CPTCAO-TBELA

05
05         WRITE ZD-REGISTRO.
05
05         PERFORM 1170-TESTAR-FS-ATLZB0H4.
05
05         ADD 1                       TO ACU-GRAV-ATLZB0H4.

      *----------------------------------------------------------------*
       3123-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA GRAVAR ARQUIVO FITCONTB                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3124-GRAVAR-FITA4               SECTION.
      *----------------------------------------------------------------*

           MOVE COBR-DATA-ORIG         OF DETALHES
                                       TO GFCTJH-DT-ENVIO-MOVTO.
           MOVE COBR-ROTINA-ORIG       OF DETALHES
                                       TO GFCTJH-CD-ROTINA-ORIGEM
           MOVE COBR-NUM-ARQ-ORIG      OF DETALHES
                                       TO GFCTJH-CD-NUMERO-ARQUIVO.
           MOVE COBR-SEQ-ARQ-ORIG      OF DETALHES
                                       TO GFCTJH-CD-SEQ-MOVTO.
           MOVE 4                      TO GFCTJH-CD-ORIGEM
                                          GFCTJH-CD-TIPO-SOLTC.
           MOVE COBR-TARIFA-ORIG       OF DETALHES
                                       TO GFCTJH-TARIFA.
           MOVE WRK-VLR-COBRADO-EVENTO
                                       TO GFCTJH-VALOR.
           MOVE COBR-BANCO-DEB         OF DETALHES
                                       TO GFCTJH-BANCO.
           MOVE COBR-AGENCIA-DEB       OF DETALHES
                                       TO GFCTJH-AGENCIA.
           MOVE COBR-CONTA-DEB         OF DETALHES
                                       TO GFCTJH-CONTA.
           MOVE COBR-BANCO-ORIG        OF DETALHES
                                       TO GFCTJH-BANCO-ORIGEM.
           MOVE COBR-AGENCIA-ORIG      OF DETALHES
                                       TO GFCTJH-AGENCIA-ORIGEM.
           MOVE COBR-DATA-EVNTO-ORIG   OF DETALHES
                                       TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-DB2.
           MOVE WRK-MES-AUX            TO WRK-MES-DB2.
           MOVE WRK-ANO-AUX            TO WRK-ANO-DB2.
           MOVE WRK-DATA-DB2           TO GFCTJH-DATA-EVENTO.

      *    DISPLAY 'REGISTRO GRAVADO NA FITA CONTABIL'
      *    DISPLAY  GFCTJH-ENTRADA.

           WRITE GFCTJH-ENTRADA.

           PERFORM 1150-TESTAR-FS-FITCONTB.

           ADD 1                       TO ACU-GRAV-FITCONTB.
070710
06         PERFORM 3232-GRAVAR-ARQDEBIT.

      *----------------------------------------------------------------*
       3124-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA EVENTO COBRADO PARCIAL                              *
      ******************************************************************
      *----------------------------------------------------------------*
       3130-EVENTO-COBRADO-PARCIAL     SECTION.
      *----------------------------------------------------------------*

           IF  QTENTV-DEB-MOVTO     OF GFCTB092
070705                              NOT LESS QMAX-TENTV-DEB OF GFCTB092
01             PERFORM 3131-FIM-REPIQUE
           ELSE
               PERFORM 3132-REPIQUE
           END-IF.

      *----------------------------------------------------------------*
       3130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA FIM REPIQUE                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       3131-FIM-REPIQUE                SECTION.
      *----------------------------------------------------------------*

WS    *    DISPLAY 'FIM REPIQUE'
WS    *    DISPLAY 'COBR-DEB-PARCIAL   =' COBR-DEB-PARCIAL  OF  DETALHES
WS    *    DISPLAY 'COBR-PARC-ULT-REPIQ=' COBR-PARCIAL-ULT-REPIQ
WS    *                                   OF  DETALHES

           MOVE 'N'                      TO WRK-GRAVA-CONTABIL.
070710
09         IF VTARIF-DEB-MOVTO OF GFCTB092
09                                        = ZEROS         AND
09            WRK-VLR-ATUALIZA            = ZEROS
09            MOVE 12                    TO WRK-CSIT-EVNTO-5525
09         ELSE
09            MOVE 04                    TO WRK-CSIT-EVNTO-5525
09         END-IF.

           IF  COBR-DEB-PARCIAL        OF  DETALHES
                                       EQUAL 1   AND
              (COBR-PARCIAL-ULT-REPIQ  OF  DETALHES
NC1706                                 EQUAL '1' OR 'S')  AND
               WRK-VLR-ATUALIZA        GREATER ZEROS
               COMPUTE WRK-VLR-DETALHES = WRK-VLR-ATUALIZA +
                                          VTARIF-DEB-MOVTO OF GFCTB092
               MOVE ZEROS              TO WRK-VLR-ATUALIZA
               MOVE 'S'                  TO WRK-GRAVA-CONTABIL
      *        DISPLAY ' COBRANCA PARCIAL NO ULTIMO REPIQUE'
      *    ELSE
      *        DISPLAY ' SEM COBR PARCIAL NO ULTIMO REPIQUE'
           END-IF.

           MOVE GFCT06-DPROCM-ANTER    TO WRK-DATA-DEB.

           PERFORM 3121-ATUALIZA-GFCTB092.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 3123-ATUALIZAR-SIT-EVENTO
               IF  WRK-GRAVA-CONTABIL      = 'S'
070710             PERFORM 3124-GRAVAR-FITA4
10                 PERFORM 3133-GRAVAR-COB-PARCIAL
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3131-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA REPIQUE                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3132-REPIQUE                    SECTION.
      *----------------------------------------------------------------*

      *    DISPLAY ' REPIQUE'
      *    DISPLAY ' COBR-VALOR-MINIMO =' COBR-VALOR-MINIMO OF DETALHES
      *    DISPLAY ' COBR-DEB-PARCIAL  =' COBR-DEB-PARCIAL OF DETALHES.

           MOVE GFCT06-DPROCM-ANTER        TO WRK-DATA-DEB.

080603     MOVE 03                         TO WRK-CSIT-EVNTO-5525.
080603     ADD   1                         TO QTENTV-DEB-MOVTO
                                                            OF GFCTB092.

080603*----------------->> VALOR MINIMO
           IF  COBR-VALOR-MINIMO       OF DETALHES
                                        > ZEROS
               COMPUTE WRK-VLR-RESTANTE = VTARIF-LIQ-MOVTO OF GFCTB092 -
                                        ( VTARIF-DEB-MOVTO OF GFCTB092 +
                                          WRK-VLR-ATUALIZA )
               IF  COBR-VALOR-MINIMO   OF DETALHES
                                        > WRK-VLR-RESTANTE
                   COMPUTE
                       WRK-VLR-DETALHES = WRK-VLR-ATUALIZA +
                                          VTARIF-DEB-MOVTO OF GFCTB092
                   MOVE 04             TO WRK-CSIT-EVNTO-5525
                   IF  WRK-VLR-ATUALIZA > ZEROS
                       MOVE 'S'        TO WRK-GRAVA-CONTABIL
090604                 MOVE ZEROS      TO WRK-VLR-ATUALIZA
                   END-IF
      *************DISPLAY 'ENCERROU REPIQUE POR VALOR MINIMO'
                   GO                  TO 3132-CONTINUA
               END-IF
0708       END-IF.

      *----------------->> DEBITO PARCIAL
           IF  COBR-DEB-PARCIAL            OF DETALHES
                                           EQUAL 1
               COMPUTE WRK-VLR-DETALHES    = WRK-VLR-ATUALIZA +
                                           VTARIF-DEB-MOVTO OF GFCTB092
               MOVE ZEROS                  TO WRK-VLR-ATUALIZA
               MOVE 'S'                    TO WRK-GRAVA-CONTABIL
      *        DISPLAY 'ENTROU EM REPIQUE COM COBRANCA PARCIAL'
           ELSE
               MOVE 'N'            TO WRK-GRAVA-CONTABIL
      *        DISPLAY 'ENTROU EM REPIQUE SEM COBRANCA PARCIAL'
           END-IF.

       3132-CONTINUA.

           PERFORM 3121-ATUALIZA-GFCTB092.

           IF  SQLCODE                     EQUAL ZEROS
070710         IF (COBR-VALOR-DEBITADO     OF ATUALIZA
070710                                     GREATER ZEROS AND
07                 WRK-GRAVA-CONTABIL      EQUAL 'S'     AND
                   RETURN-CODE             EQUAL ZEROS   AND
                   WRK-5525-COD-RETORNO    EQUAL ZEROS)
070710             PERFORM 3123-ATUALIZAR-SIT-EVENTO
                   PERFORM 3124-GRAVAR-FITA4
                   PERFORM 3133-GRAVAR-COB-PARCIAL
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3132-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA GRAVAR-COB-PARCIAL                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3133-GRAVAR-COB-PARCIAL         SECTION.
      *----------------------------------------------------------------*
      *    DISPLAY ' GRAVAR PARCIAL'

071130*---INIC--> SUBSTITUIR INSERT NA H4 POR GRAVACAO DE ARQUIVO <-----
071130     MOVE COBR-DATA-ORIG         OF DETALHES
071130                                 TO GFCT68-DENVIO-MOVTO-TARIF
071130     MOVE COBR-ROTINA-ORIG       OF DETALHES
071130                                 TO GFCT68-CROTNA-ORIGE-MOVTO
071130     MOVE COBR-NUM-ARQ-ORIG      OF DETALHES
071130                                 TO GFCT68-CNRO-ARQ-MOVTO
071130     MOVE COBR-SEQ-ARQ-ORIG      OF DETALHES
071130                                 TO GFCT68-CSEQ-MOVTO
071130     MOVE WRK-VLR-COBRADO-EVENTO
071130                                 TO GFCT68-VDEB-PCIAL-TARIF
071130     MOVE GFCT06-DPROCM-ANTER    TO GFCT68-DDEB-PCIAL-TARIF
071130
071130     WRITE GFCT68-GFCTB0H7.
071130
071130     PERFORM 1194-TESTAR-FS-ARQATUH7.
071130
071130     ADD 1                       TO ACU-GRAV-ARQATUH7.

071130*****EXEC SQL
      *      INSERT INTO DB2PRD.TDEB_PCIAL_TARIF
      *           (DENVIO_MOVTO_TARIF,
      *            CROTNA_ORIGE_MOVTO,
      *            CNRO_ARQ_MOVTO,
      *            CSEQ_MOVTO,
      *            DDEB_PCIAL_TARIF,
      *            VDEB_PCIAL_TARIF)
      *        VALUES
      *           (:GFCTB0H7.DENVIO-MOVTO-TARIF,
      *            :GFCTB0H7.CROTNA-ORIGE-MOVTO,
      *            :GFCTB0H7.CNRO-ARQ-MOVTO,
      *            :GFCTB0H7.CSEQ-MOVTO,
      *            :GFCTB0H7.DDEB-PCIAL-TARIF,
      *            :GFCTB0H7.VDEB-PCIAL-TARIF)
      *    END-EXEC.
      *
      *    IF (SQLCODE                 NOT EQUAL ZEROS) OR
      *       (SQLWARN0                EQUAL 'W'      )
VINI  *        MOVE COBR-RETORNO OF ATUALIZA
VINI  *                                TO COBR-RETORNO    OF DETALHES
VINI  *        MOVE REG-COBRANCA  OF DETALHES
VINI  *                                TO REG-COBRANCA    OF OCOR1182
070705*
02    *        MOVE 14             TO COBR-SITUACAO-EVENTO OF OCOR1182
      *        MOVE 'GFCT0520'         TO COBR-JOB         OF OCOR1182
      *        MOVE 'GFCT1182'         TO COBR-PROGRAMA    OF OCOR1182
      *        MOVE 'GFCTB0H7'         TO COBR-RECURSO     OF OCOR1182
      *        MOVE SQLCODE            TO WRK-SQLCODE
      *        MOVE WRK-SQLCODE        TO COBR-COD-RETORNO OF OCOR1182
      *        MOVE 'ERRO NO INSERT DA TABELA'
      *                                TO COBR-DESCRICAO   OF OCOR1182
      *        WRITE REG-COBRANCA      OF OCOR1182
      *        PERFORM 1160-TESTAR-FS-OCOR1182
      *        ADD 1                   TO ACU-GRAV-OCOR1182
      *    END-IF.
      *
      *    DISPLAY 'GRAVA COBR PARCIAL  SQL '  SQLCODE.
      *    DISPLAY 'CSEQ-MOVTO       =' CSEQ-MOVTO OF GFCTB0H7
      *    DISPLAY 'DDEB-PCIAL-TARIF =' DDEB-PCIAL-TARIF
      *    DISPLAY 'VDEB-PCIAL-TARIF =' VDEB-PCIAL-TARIF.
071130*---FIM --> SUBSTITUIR INSERT NA H4 POR GRAVACAO DE ARQUIVO <-----

      *----------------------------------------------------------------*
       3133-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
070705
04    ******************************************************************
04    * ROTINA PARA NAO ATUALIZAR SIT EVENTO NO FERIADO AGENCIA POUP   *
04    ******************************************************************
04    *----------------------------------------------------------------*
04     3140-RETORNAR-SIT-EVENTO        SECTION.
04    *----------------------------------------------------------------*
04         MOVE WRK-DEFETV-DEB-MOVTO      TO  WRK-DATA-DEB.
04
           IF  COBR-COD-RETORNO   OF ATUALIZA EQUAL '009'
               MOVE 3                    TO  WRK-RETORNO-F
           ELSE
              IF   WRK-DEFETV-DEB-MOVTO       = '01.01.0001'
                   MOVE 2                    TO  WRK-RETORNO-F
              ELSE
                   MOVE 3                    TO  WRK-RETORNO-F
              END-IF
           END-IF.
04
04         PERFORM 3121-ATUALIZA-GFCTB092.
04         ADD     1                      TO  ACU-FERIADO-AGEN.
04
04    *----------------------------------------------------------------*
04     3140-99-FIM.                    EXIT.
04    *----------------------------------------------------------------*

070920******************************************************************
070920* ROTINA PARA GRAVAR ARQCOBR2                                    *
070920******************************************************************
070920*----------------------------------------------------------------*
070920 3212-GRAVAR-ARQCOBR2            SECTION.
070920*----------------------------------------------------------------*
070920
070920*    MOVE REG-COBRANCA     OF ATUALIZA TO REG-GFCTMW.
030408     MOVE REG-COBRANCA     OF DETALHES TO REG-GFCTMW.
070920     MOVE QMAX-TENTV-DEB   OF GFCTB092 TO GFCTMW-TENTV-DEB-MAX.
070920     MOVE VTARIF-LIQ-MOVTO OF GFCTB092 TO GFCTMW-VALOR-LIQ92.
070920     MOVE QTENTV-DEB-MOVTO OF GFCTB092 TO GFCTMW-TENTV-DEB-ATU.
070920     MOVE VTARIF-DEB-MOVTO OF GFCTB092 TO GFCTMW-VALOR-COBR92.
070920     MOVE CSIT-EVNTO-RECBD OF GFCTB092 TO GFCTMW-SITUACAO-EVENTO.

           IF WRK-VR-SALDO                   NOT LESS
              COBR-VALOR-DEBITAR OF DETALHES
              MOVE COBR-VALOR-DEBITAR OF DETALHES TO
                   GFCTMW-VALOR-DEBITADO
              MOVE WRK-VR-SALDO TO WRK-VR-SALDO-PK
              MOVE COBR-VALOR-DEBITAR OF DETALHES TO
              WRK-COBR-VALOR-DEBITAR-PK
              SUBTRACT WRK-COBR-VALOR-DEBITAR-PK FROM
              WRK-VR-SALDO-PK
              MOVE WRK-VR-SALDO-PK TO WRK-VR-SALDO
           ELSE
              MOVE WRK-VR-SALDO TO
                   GFCTMW-VALOR-DEBITADO
              MOVE ZEROS TO WRK-VR-SALDO
           END-IF.

070920
070920     WRITE REG-GFCTMW.
070920
070920     PERFORM 1190-TESTAR-FS-ARQCOBR2.
070920
070920     ADD 1                       TO ACU-GRAV-ARQCOBR2.
070920
070920*----------------------------------------------------------------*
070920 3212-99-FIM.                    EXIT.
070920*----------------------------------------------------------------*


06    ******************************************************************
06    * ROTINA PARA GRAVAR ARQUIVO DE EVENTOS EFETIVAMENTE COBRADOS    *
06    ******************************************************************
06    *----------------------------------------------------------------*
06     3232-GRAVAR-ARQDEBIT            SECTION.
06    *----------------------------------------------------------------*
06
06         MOVE COBR-AGENCIA-DEB       OF DETALHES
06                                     TO GFCTZJ-COD-AGENCIA.
06         MOVE COBR-CONTA-DEB         OF DETALHES
06                                     TO GFCTZJ-NUM-CONTA.
06         MOVE COBR-GRUPO-DEB         OF DETALHES
06                                     TO GFCTZJ-COD-GRUPO.
06         MOVE COBR-SUBGRUPO-DEB      OF DETALHES
06                                     TO GFCTZJ-COD-SUB-GRUPO.
06         MOVE COBR-CGC-CPF-MUNERO    OF DETALHES
06                                     TO GFCTZJ-CNPJ-CPF-PRINCIPAL.
06         MOVE COBR-CGC-CPF-FILIAL    OF DETALHES
06                                     TO GFCTZJ-CNPJ-CPF-FILIAL.
06         MOVE COBR-CGC-CPF-CONTROLE  OF DETALHES
06                                     TO GFCTZJ-CNPJ-CPF-CONTROLE.
06         MOVE COBR-TARIFA-ORIG       OF DETALHES
06                                     TO GFCTZJ-COD-EVENTO-TARIFA.
06         MOVE DEFETV-DEB-MOVTO       OF GFCTB092
06                                     TO WRK-DATA-DEB.
06         MOVE WRK-DIA-DEB            TO GFCTZJ-DIA-DEBITO.
06         MOVE WRK-MES-DEB            TO GFCTZJ-MES-DEBITO.
06         MOVE WRK-ANO-DEB            TO GFCTZJ-ANO-DEBITO.
06         MOVE ZEROS                  TO GFCTZJ-COD-IDENT-COBRANCA.
06         MOVE WRK-VLR-COBRADO-EVENTO TO GFCTZJ-VLR-COBRADO.

070710
08         MOVE DOCOR-EVNTO            OF GFCTB092
08                                     TO WRK-DATA-EVT.
08         MOVE WRK-DIA-EVT            TO GFCTZJ-DIA-EVENTO.
08         MOVE WRK-MES-EVT            TO GFCTZJ-MES-EVENTO.
08         MOVE WRK-ANO-EVT            TO GFCTZJ-ANO-EVENTO.

06         IF   COBR-COD-RETORNO       OF ATUALIZA
06                                     IS NUMERIC
06              MOVE 1                 TO GFCTZJ-COD-IDENT-ORIG-DEB
06         ELSE
06              MOVE 2                 TO GFCTZJ-COD-IDENT-ORIG-DEB.
06
06         WRITE GFCTZJ-REGISTRO.
06
06         PERFORM 1180-TESTAR-FS-ARQDEBIT.
06
06         ADD 1                       TO ACU-GRAV-ARQDEBIT.
06
06    *----------------------------------------------------------------*
06     3232-99-FIM.                    EXIT.
06    *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA PROCEDIMENTOS FINAIS                                *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

071130*    IF  ACU-COMMIT              GREATER ZEROS
071130*        PERFORM 3122-EFETUAR-COMMIT
071130*    END-IF.

           PERFORM 4100-EMITIR-DISPLAY.

           CLOSE ATUALIZA
                 DETALHES
                 CTRLDATA
                 SAIDETAL
070710           ATLZB0H4
05               FITCONTB
070710           OCOR1182
071129           ARQCOBR2
06               ARQDEBIT
071130           ARQATU92
071130           ARQATUH7.



           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           GOBACK.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA EMITIR DISPLAY                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS-ATUALIZA     TO WRK-MASCARA.

           DISPLAY '******************** GFCT1182 ********************'.
           DISPLAY '*                                                *'.
           DISPLAY '* REGISTROS LIDOS EM ATUALIZA...: ' WRK-MASCARA
                                                                   ' *'.

           MOVE ACU-LIDOS-DETALHES     TO WRK-MASCARA.

           DISPLAY '* REGISTROS LIDOS EM DETALHES...: ' WRK-MASCARA
                                                                   ' *'.

           MOVE ACU-LIDOS-CTRLDATA     TO WRK-MASCARA.

           DISPLAY '* REGISTROS LIDOS EM CTRLDATA...: ' WRK-MASCARA
                                                                   ' *'.
           MOVE ACU-GRAV-SAIDETAL      TO WRK-MASCARA.

           DISPLAY '* REGISTROS GRAVADOS EM SAIDETAL: ' WRK-MASCARA
                                                                   ' *'.

           MOVE ACU-GRAV-FITCONTB      TO WRK-MASCARA.

           DISPLAY '* REGISTROS GRAVADOS EM FITCONTB: ' WRK-MASCARA
                                                                   ' *'.

           MOVE ACU-GRAV-OCOR1182      TO WRK-MASCARA.

           DISPLAY '* REGISTROS GRAVADOS EM OCOR1182: ' WRK-MASCARA
                                                                   ' *'.
070710
05         MOVE ACU-GRAV-ATLZB0H4      TO WRK-MASCARA.
05
05         DISPLAY '* REGISTROS GRAVADOS EM GFCTB0H4: ' WRK-MASCARA
05                                                                 ' *'.

           MOVE ACU-DESPREZADOS        TO WRK-MASCARA.

           DISPLAY '* REGISTROS DESPREZADOS.........: ' WRK-MASCARA
                                                                   ' *'.

071130     MOVE ACU-GRAV-ARQATU92      TO WRK-MASCARA.

071130     DISPLAY '* REGISTROS A ATUALIZAR GFCTB092: ' WRK-MASCARA
                                                                   ' *'.
071130     MOVE ACU-GRAV-ARQATUH7      TO WRK-MASCARA.

071130     DISPLAY '* REGISTROS A ATUALIZAR GFCTB0H7: ' WRK-MASCARA
                                                                   ' *'.

070705
04         MOVE ACU-FERIADO-AGEN       TO WRK-MASCARA.
04
04         DISPLAY '* FERIADO NA AGENCIA POUP.......: ' WRK-MASCARA
04                                                                 ' *'.
070710
06         MOVE ACU-GRAV-ARQDEBIT      TO WRK-MASCARA.
06
06         DISPLAY '* REGISTROS GRAVADOS EM ARQDEBIT: ' WRK-MASCARA
06                                                                 ' *'.
071129     MOVE ACU-GRAV-ARQCOBR2      TO WRK-MASCARA.
071129
071129     DISPLAY '* REGISTROS GRAVADOS EM ARQCOBR2: ' WRK-MASCARA
071129                                                             ' *'.

070705     MOVE ACU-NF-DETALHES        TO WRK-MASCARA.
03
03         DISPLAY '* REG. CONSOLIDADO SEM DETALHES.: ' WRK-MASCARA
03                                                                 ' *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT1182 ********************'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA PROCESSAR ROTINA ERRO                               *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 4                      TO RETURN-CODE.
           MOVE 'GFCT1182'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
