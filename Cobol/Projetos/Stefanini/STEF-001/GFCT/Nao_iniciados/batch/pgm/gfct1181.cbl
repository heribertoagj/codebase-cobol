      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1181.
       AUTHOR.     WAGNER SILVA.
      *================================================================*
      *                    C P M  -  S I S T E M A S.                  *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1181                                    *
      *    ANALISTA....:   WAGNER SILVA            - PROCKWORK GP. 50  *
      *    DATA........:   07/02/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   ATUALIZAR REGISTROS COBRADOS - COBRANCA     *
      *                    AGRUPAMENTO CONTABIL                        *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                    ATUALIZA                  I#GFCTIL          *
      *                    DETALHES                  I#GFCTIL          *
      *                    CTRLDATA                  I#GFCT06          *
      *                    SAIDETAL                  I#GFCTIL          *
      *                    FITCONTB                  I#GFCTJH          *
      *                    OCOR1181                  I#GFCTIL          *
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
      *    PROGRAMA....:   ALTERACAO                                   *
      *    ANALISTA....:   WAGNER SILVA            - PROCKWORK GP. 50  *
      *    DATA........:   17/01/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   ATUALIZAR AG/CTA DEBITO OU CENTRALIZADORA,  *
      *                    EM CASO DE TRAG NO MORA/POUP                *
      *----------------------------------------------------------------*
      *================================================================*
      *                        A L T E R A C A O                       *
      *================================================================*
      *    ANALISTA....:   WAGNER SILVA            - PROCKWORK GP. 50  *
      *    DATA........:   16/04/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   ACERTOS DIVERSOS                            *
      *     01 - ZERAR VLR VINDO DO MORA SE COD-RETORNO = 001 OU 012   *
      *     02 - SE FIM DO REPIQUE SEM COBRAR NADA, STATUS = 12        *
      *     03 - GRAVAR TAB H7 SE COBRANCA TOTAL E VL DEBITAR DIFERENTE*
      *          DO VL LIQUIDO DA 92 (FIM DE REPIQUE C/ COBRACA TOTAL) *
      *     04 - MOVER 14 P/ SITUACAO-EVENTO  EM CASO DE ERRO          *
      *     05 - SUBSTITUIR ACESSO DA GFCT5525 POR UPDATE E ARQUIVO    *
      *     06 - DISPLAY SE REG ATUALIZA NAO ENCONTRADO NO DETALHES    *
      *          LIMITADO A 1000 OCORRENCIAS                           *
      *----------------------------------------------------------------*
      *================================================================*
      *=                      A L T E R A C A O                       =*
      *================================================================*
      *    ANALISTA....:   WAGNER SILVA            - PROCKWORK GP. 50  *
      *    DATA........:   18/05/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVOS...:                                               *
      *               10 - INCLUIR TRATAMENTO PARA COBR-COD-RETORNO=F  *
      *                    RETORNAR SITUACAO EVENTO DA 92 P/ SITUACAO  *
      *                    ANTERIOR A COBRANCA                         *
      *               11 - INCLUIR ARQUIVO CONTENDO INFORMACOES SOBRE  *
      *                    REGISTROS COBRADOS                          *
      *               12 - FORMATAR DATA DE OCORRENCIA DO EVENTO NA    *
      *                    INC I#GFCTZJ                                *
      *----------------------------------------------------------------*
070614*================================================================*
070614*    PROGRAMA....:   ALTERACAO                                   *
070614*    ANALISTA....:   VINICIUS C. MADUREIRA - PROCKWORK GP. 50    *
070614*    DATA........:   14/06/2007                                  *
070614*----------------------------------------------------------------*
070614*    OBJETIVO....:   DEIXAR DE GRAVAR GFCTB0H4 EM CASO DE REPIQUE*
070614*----------------------------------------------------------------*
070703*================================================================*
070703*    PROGRAMA....:   ALTERACAO                                   *
070703*    ANALISTA....:   VINICIUS C. MADUREIRA - PROCKWORK GP. 50    *
070703*    DATA........:   14/06/2007                                  *
070703*----------------------------------------------------------------*
070703*    OBJETIVO....:   DEIXAR DE GRAVAR GFCTB0H4 EM CASO DE REPIQUE*
070703*                    QUANDO VALOR DEBITADO IGUAL A 0.            *
070703*----------------------------------------------------------------*
071204*================================================================*
071204*=                      A L T E R A C A O                       =*
071204*================================================================*
071204*    ANALISTA....:   VINICIUS C. MADUREIRA - PROCWORK GP. 50     *
071204*    DATA........:   04/12/2007                                  *
071204*----------------------------------------------------------------*
071204*    OBJETIVO....:   INCLUIR ARQUIVO CONTENDO INFORMACOES SOBRE  *
071204*                    REGISTROS COBRADOS                          *
071204*================================================================*
071205*================================================================*
071205*=                      A L T E R A C A O                       =*
071205*================================================================*
071205*   ANALISTA....:   VINICIUS C. MADUREIRA - PROCWORK GP. 50      *
071205*   DATA........:   05/12/2007                                   *
071205*----------------------------------------------------------------*
071205*   OBJETIVOS...:                                                *
071205*    SUBSTITUIR UPDATE DA TABELA GFCTBO92 POR GERACAO DE ARQUIVO *
071205*    SUBSTITUIR INSERT DA TABELA GFCTBOH7 POR GERACAO DE ARQUIVO *
071205*================================================================*
030408*================================================================*
030408*=                      A L T E R A C A O                       =*
030408*================================================================*
030408*   ANALISTA....:   WAGNER SILVA - PROCWORK GP. 50               *
030408*   DATA........:   03/04/2008                                   *
030408*----------------------------------------------------------------*
030408*   OBJETIVOS...:                                                *
030408*    SUBSTITUIR REGISTRO DE ATUALIZA PELO DE DETALHES NA GERACAO *
030408*    DO ARQCOBR2                                                 *
030408*================================================================*
BI1210*=                      A L T E R A C A O                       =*
BI1210*================================================================*
BI1210*   ANALISTA....:   UBIRAJARA (BIRA) - PROCWORK GP. 50           *
BI1210*   DATA........:   20/12/2010                                   *
BI1210*----------------------------------------------------------------*
BI1210*   OBJETIVOS...:                                                *
BI1210*    MOVIMENTAR O CAMPO (COBR-NUM-PARC) DE (DETALHES) PARA       *
BI1210*    (GFCT67-NUM-PARC) NA GRAVACAO DE ARQATU92.                  *
BI1210*================================================================*
AP1203*=                      A L T E R A C A O                       =*
AP1203*================================================================*
AP1203*   ANALISTA....:   PAGNOCCA         - PROCWORK GP. 50           *
AP1203*   DATA........:   12/03/2011                                   *
AP1203*----------------------------------------------------------------*
AP1203*   OBJETIVOS...:                                                *
AP1203*    REPASSAR CODIGO DA TARIFA LIDA NA BASE GFCTB092             *
AP1203*================================================================*

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

           SELECT OCOR1181 ASSIGN      TO UT-S-OCOR1181
           FILE STATUS                 IS WRK-FS-OCOR1181.

05         SELECT ATLZB0H4 ASSIGN      TO UT-S-ATLZB0H4
05         FILE STATUS                 IS WRK-FS-ATLZB0H4.

11         SELECT ARQDEBIT  ASSIGN     TO UT-S-ARQDEBIT
11         FILE STATUS                 IS WRK-FS-ARQDEBIT.

071204     SELECT ARQCOBR2  ASSIGN     TO UT-S-ARQCOBR2
071204     FILE STATUS                 IS WRK-FS-ARQCOBR2.

071205     SELECT ARQATU92  ASSIGN     TO UT-S-ARQATU92
071205     FILE STATUS                 IS WRK-FS-ARQATU92.

071205     SELECT ARQATUH7  ASSIGN     TO UT-S-ARQATUH7
071205     FILE STATUS                 IS WRK-FS-ARQATUH7.

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

       FD  OCOR1181
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       COPY 'I#GFCTIL'.

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

11    *----------------------------------------------------------------*
11    *   OUTPUT:  REGISTROS EFETIVAMENTE DEBITADOS                    *
11    *            ORG. SEQUENCIAL     -   LRECL   =   080             *
11    *----------------------------------------------------------------*
11
11     FD  ARQDEBIT
11         RECORDING MODE IS F
11         LABEL RECORD IS STANDARD
11         BLOCK CONTAINS  0 RECORDS.
11
11     COPY 'I#GFCTZJ'.

071204*----------------------------------------------------------------*
071204*   OUTPUT:  REGISTROS EFETIVAMENTE DEBITADOS                    *
071204*            ORG. SEQUENCIAL     -   LRECL   =   340             *
071204*----------------------------------------------------------------*
071204
071204 FD  ARQCOBR2
071204     RECORDING MODE IS F
071204     LABEL RECORD IS STANDARD
071204     BLOCK CONTAINS  0 RECORDS.
071204
071204 COPY 'I#GFCTMW'.

071205*----------------------------------------------------------------*
071205*   OUTPUT:  REGISTROS PARA ATUALIZACAO NA TABELA GFCTB092       *
071205*            ORG. SEQUENCIAL     -   LRECL   =   090             *
071205*----------------------------------------------------------------*
071205
071205 FD  ARQATU92
071205     RECORDING MODE IS F
071205     LABEL RECORD IS STANDARD
071205     BLOCK CONTAINS  0 RECORDS.
071205
071205 COPY 'I#GFCT67'.

071205*----------------------------------------------------------------*
071205*   OUTPUT:  REGISTROS PARA INSERIR  NA TABELA GFCTB0H7          *
071205*            ORG. SEQUENCIAL     -   LRECL   =   038             *
071205*----------------------------------------------------------------*
071205
071205 FD  ARQATUH7
071205     RECORDING MODE IS F
071205     LABEL RECORD IS STANDARD
071205     BLOCK CONTAINS  0 RECORDS.
071205
071205 COPY 'I#GFCT68'.

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
11     77  ACU-GRAV-ARQDEBIT           PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAV-SAIDETAL           PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAV-FITCONTB           PIC  9(009) COMP-3  VALUE ZEROS.
05     77  ACU-GRAV-ATLZB0H4           PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAV-OCOR1181           PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-DESPREZADOS             PIC  9(009) COMP-3  VALUE ZEROS.
06     77  ACU-NF-DETALHES             PIC  9(009) COMP-3  VALUE ZEROS.
06     77  ACU-DETALHES-ENCONT         PIC  9(009) COMP-3  VALUE ZEROS.
10     77  ACU-FERIADO-AGEN            PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-COMMIT                  PIC  9(009) COMP-3  VALUE ZEROS.

071204 77  ACU-GRAV-ARQCOBR2           PIC  9(009) COMP-3  VALUE ZEROS.
071205 77  ACU-GRAV-ARQATU92           PIC  9(009) COMP-3  VALUE ZEROS.
071205 77  ACU-GRAV-ARQATUH7           PIC  9(009) COMP-3  VALUE ZEROS.
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** TESTES DE FILE STATUS ***'.
      *----------------------------------------------------------------*

05     77  WRK-FS-ATLZB0H4             PIC  X(002)         VALUE SPACES.
11     77  WRK-FS-ARQDEBIT             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ATUALIZA             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-DETALHES             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-CTRLDATA             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SAIDETAL             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-FITCONTB             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-OCOR1181             PIC  X(002)         VALUE SPACES.

071204 77  WRK-FS-ARQCOBR2             PIC  X(002)         VALUE SPACES.
071205 77  WRK-FS-ARQATU92             PIC  X(002)         VALUE SPACES.
071205 77  WRK-FS-ARQATUH7             PIC  X(002)         VALUE SPACES.

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
10*****77  WRK-DATA-DEB                PIC  X(010)         VALUE SPACES.
       77  WRK-DATA-AGENDAMENTO        PIC  X(010)         VALUE SPACES.
       77  WRK-VLR-DETALHES            PIC  9(011)V99      VALUE ZEROS.
       77  WRK-VLR-ATUALIZA            PIC  9(011)V99      VALUE ZEROS.
       77  WRK-VLR-RESTANTE            PIC  9(011)V99      VALUE ZEROS.
       77  WRK-VLR-COBRADO-EVENTO      PIC  9(011)V99      VALUE ZEROS.
       77  WRK-VR-SALDO                PIC  9(011)V99      VALUE ZEROS.
       77  WRK-VR-SALDO-PK             PIC  9(011)V99 COMP-3 VALUE 0.
       77  WRK-COBR-VALOR-DEBITAR-PK   PIC  9(011)V99 COMP-3 VALUE 0.
       77  WRK-CSIT-EVNTO-5525         PIC  9(002)         VALUE ZEROS.
       77  WRK-GRAVA-CONTABIL          PIC  X(001)         VALUE SPACES.
10     77  WRK-RETORNO-F               PIC  9(002)         VALUE ZEROS.
AP1203 01  WRK-CSERVC-TARIF            PIC +9(005)         VALUE ZEROS.
AP1203 01  FILLER                      REDEFINES
AP1203     WRK-CSERVC-TARIF.
AP1203     05  FILLER                  PIC  X(001).
AP1203     05  WRK-CSERVC-TARIF-RS     PIC  9(005).

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA VARIAVEIS DE DATAS ***'.
      *----------------------------------------------------------------*

10     01  WRK-DEFETV-DEB-MOVTO        PIC  X(010)         VALUE SPACES.

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

11     01  WRK-DATA-DEB.
11         03  WRK-DIA-DEB             PIC  9(002)         VALUE ZEROS.
11         03  FILLER                  PIC  X(001)         VALUE '.'.
11         03  WRK-MES-DEB             PIC  9(002)         VALUE ZEROS.
11         03  FILLER                  PIC  X(001)         VALUE '.'.
11         03  WRK-ANO-DEB             PIC  9(004)         VALUE ZEROS.

12     01  WRK-DATA-EVT.
12         03  WRK-DIA-EVT             PIC  9(002)         VALUE ZEROS.
12         03  FILLER                  PIC  X(001)         VALUE '.'.
12         03  WRK-MES-EVT             PIC  9(002)         VALUE ZEROS.
12         03  FILLER                  PIC  X(001)         VALUE '.'.
12         03  WRK-ANO-EVT             PIC  9(004)         VALUE ZEROS.

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


PCT    01 WRK-CPTCAO-GFCTB0H4          PIC  X(026) VALUE SPACES.
       01 FILLER                       REDEFINES WRK-CPTCAO-GFCTB0H4.
           05 FILLER                   PIC X(023).
PCT        05 WRK-CPTCAO-B0H4          PIC 9(003).

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

071205*    EXEC SQL
071205*        INCLUDE GFCTB0H7
071205*    END-EXEC.

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
                   COBR-CHAVE-COBRANCA OF DETALHES EQUAL HIGH-VALUES
06                                     AND
06                 COBR-CHAVE-COBRANCA OF ATUALIZA EQUAL HIGH-VALUES.

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
                       FITCONTB
                       OCOR1181
05                     ATLZB0H4
11                     ARQDEBIT
071204                 ARQCOBR2
071205                 ARQATU92
071205                 ARQATUH7.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

071205*    PERFORM 1010-RESTART.
           PERFORM 1100-TESTAR-FILE-STATUS.
05         PERFORM 1200-FORMATAR-TIMESTAMP-H4.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

071205*----------------------------------------------------------------*
071205*1010-RESTART                    SECTION.
071205*----------------------------------------------------------------*
071205*
071205***** VERIFICAR SE ROTINA DEVE SER INCLUIDA.
071205***** VAI PERDER MUITO TEMPO PARA CINCRONIZAR 2 TAB. E 5 ARQ.
071205***** EM CASO DE ABEND MELHOR RODAR NOVAMENTE O JOB.
071205*
071205*----------------------------------------------------------------*
071205*1010-99-FIM.                    EXIT.
071205*----------------------------------------------------------------*

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

           PERFORM 1160-TESTAR-FS-OCOR1181.

05         PERFORM 1170-TESTAR-FS-ATLZB0H4.

11         PERFORM 1180-TESTAR-FS-ARQDEBIT.

071205     PERFORM 1190-TESTAR-FS-ARQCOBR2.

071204     PERFORM 1192-TESTAR-FS-ARQATU92.

071204     PERFORM 1194-TESTAR-FS-ARQATUH7.

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
       1150-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA TESTE DE FILE STATUS DO ARQUIVO OCOR1181            *
      ******************************************************************
      *----------------------------------------------------------------*
       1160-TESTAR-FS-OCOR1181         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-OCOR1181         NOT EQUAL '00'
               MOVE WRK-FS-OCOR1181    TO WRK-FILE-STATUS
               MOVE 'OCOR1181'         TO WRK-ARQUIVO
               MOVE WRK-MSG-FS         TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1160-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

05    ******************************************************************
05    *ROTINA PARA TESTE DE FILE STATUS DO ARQUIVO ATLZB0H4            *
05    ******************************************************************
05    *----------------------------------------------------------------*
05     1170-TESTAR-FS-ATLZB0H4         SECTION.
05    *----------------------------------------------------------------*
05
05         IF  WRK-FS-ATLZB0H4         NOT EQUAL '00'
05             MOVE WRK-FS-ATLZB0H4    TO WRK-FILE-STATUS
05             MOVE 'OCOR1181'         TO WRK-ARQUIVO
05             MOVE WRK-MSG-FS         TO ERR-TEXTO
05             MOVE 'APL'              TO ERR-TIPO-ACESSO
05             PERFORM 9999-PROCESSAR-ROTINA-ERRO
05         END-IF.
05
05    *----------------------------------------------------------------*
05     1170-99-FIM.                    EXIT.
05    *----------------------------------------------------------------*

11    ******************************************************************
11    * TESTAR FILE STATUS DO ARQUIVO ARQDEBIT                         *
11    ******************************************************************
11    *----------------------------------------------------------------*
11     1180-TESTAR-FS-ARQDEBIT         SECTION.
11    *----------------------------------------------------------------*
11
11         IF  WRK-FS-ARQDEBIT         NOT EQUAL '00'
11             MOVE WRK-FS-ARQDEBIT    TO WRK-FILE-STATUS
11             MOVE 'ARQDEBIT'         TO WRK-ARQUIVO
11             MOVE WRK-MSG-FS         TO ERR-TEXTO
11             MOVE 'APL'              TO ERR-TIPO-ACESSO
11             PERFORM 9999-PROCESSAR-ROTINA-ERRO
11         END-IF.
11
11    *----------------------------------------------------------------*
11     1180-99-FIM.                    EXIT.
11    *----------------------------------------------------------------*

071204******************************************************************
071204* TESTAR FILE STATUS DO ARQUIVO ARQCOBR2                         *
071204******************************************************************
071204*----------------------------------------------------------------*
071204 1190-TESTAR-FS-ARQCOBR2         SECTION.
071204*----------------------------------------------------------------*
071204
071204     IF  WRK-FS-ARQCOBR2         NOT EQUAL '00'
071204         MOVE WRK-FS-ARQCOBR2    TO WRK-FILE-STATUS
071204         MOVE 'ARQCOBR2'         TO WRK-ARQUIVO
071204         MOVE WRK-MSG-FS         TO ERR-TEXTO
071204         MOVE 'APL'              TO ERR-TIPO-ACESSO
071204         PERFORM 9999-PROCESSAR-ROTINA-ERRO
071204     END-IF.
071204
071204*----------------------------------------------------------------*
071204 1190-99-FIM.                    EXIT.
071204*----------------------------------------------------------------*

071205******************************************************************
071205* TESTAR FILE STATUS DO ARQUIVO ARQATU92                         *
071205******************************************************************
071205*----------------------------------------------------------------*
071205 1192-TESTAR-FS-ARQATU92         SECTION.
071205*----------------------------------------------------------------*
071205
071205     IF  WRK-FS-ARQATU92         NOT EQUAL '00'
071205         MOVE WRK-FS-ARQATU92    TO WRK-FILE-STATUS
071205         MOVE 'ARQATU92'         TO WRK-ARQUIVO
071205         MOVE WRK-MSG-FS         TO ERR-TEXTO
071205         MOVE 'APL'              TO ERR-TIPO-ACESSO
071205         PERFORM 9999-PROCESSAR-ROTINA-ERRO
071205     END-IF.
071205
071205*----------------------------------------------------------------*
071205 1192-99-FIM.                    EXIT.
071205*----------------------------------------------------------------*

071205******************************************************************
071205* TESTAR FILE STATUS DO ARQUIVO ARQATUH7                         *
071205******************************************************************
071205*----------------------------------------------------------------*
071205 1194-TESTAR-FS-ARQATUH7         SECTION.
071205*----------------------------------------------------------------*
071205
071205     IF  WRK-FS-ARQATUH7         NOT EQUAL '00'
071205         MOVE WRK-FS-ARQATUH7    TO WRK-FILE-STATUS
071205         MOVE 'ARQATUH7'         TO WRK-ARQUIVO
071205         MOVE WRK-MSG-FS         TO ERR-TEXTO
071205         MOVE 'APL'              TO ERR-TIPO-ACESSO
071205         PERFORM 9999-PROCESSAR-ROTINA-ERRO
071205     END-IF.
071205
071205*----------------------------------------------------------------*
071205 1194-99-FIM.                    EXIT.
071205*----------------------------------------------------------------*

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

06    *    IF  COBR-CHAVE-COBRANCA     OF ATUALIZA
06    *                                EQUAL HIGH-VALUES
06    *        DISPLAY '***************** GFCT1181 *****************'
06    *        DISPLAY '*                                          *'
06    *        DISPLAY '*          ARQUIVO ATUALIZA VAZIO          *'
06    *        DISPLAY '*         PROCESSAMENTO  ENCERRADO         *'
06    *        DISPLAY '*                                          *'
06    *        DISPLAY '***************** GFCT1181 *****************'
06    *        PERFORM 4000-FINALIZAR
06    *    END-IF.

           PERFORM 2200-LER-DETALHES.

           IF  COBR-CHAVE-COBRANCA     OF DETALHES
                                       EQUAL HIGH-VALUES
               DISPLAY '***************** GFCT1181 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*          ARQUIVO DETALHES VAZIO          *'
               DISPLAY '*         PROCESSAMENTO  ENCERRADO         *'
               DISPLAY '*                                          *'
               DISPLAY '***************** GFCT1181 *****************'
               PERFORM 4000-FINALIZAR
           END-IF.

           PERFORM 2300-LER-CTRLDATA.

           IF  WRK-FS-CTRLDATA         EQUAL '10'
               DISPLAY '***************** GFCT1181 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*          ARQUIVO CTRLDATA VAZIO          *'
               DISPLAY '*         PROCESSAMENTO  ENCERRADO         *'
               DISPLAY '*                                          *'
               DISPLAY '***************** GFCT1181 *****************'
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
06                 ADD   1                     TO ACU-DETALHES-ENCONT
               ELSE
06                 IF  ACU-DETALHES-ENCONT      = ZEROS
06                     ADD 1                   TO ACU-NF-DETALHES
06                     IF  ACU-NF-DETALHES      < 1001
06                         DISPLAY 'REGISTRO AGRUPADO SEM DETALHES ='
06                         COBR-CHAVE-COBRANCA OF ATUALIZA
06                     END-IF
06                 END-IF
                   PERFORM 2100-LER-ATUALIZA
06                 MOVE  0                     TO ACU-DETALHES-ENCONT
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
               GO TO 3100-99-FIM
           END-IF.

           IF  COBR-COD-RETORNO           OF ATUALIZA
                                          EQUAL '000' OR '010' OR
01                                              '001' OR '012' OR 'P'
                                                               OR 'I'
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
10                     PERFORM 3400-RETORNAR-SIT-EVENTO
                   ELSE
                       MOVE REG-COBRANCA  OF DETALHES
                                          TO REG-COBRANCA    OF OCOR1181
WS    *                DISPLAY 'OCORRENCIA-RETORNO ATUALIZA NAO PREVIST'
                       MOVE 'GFCT0518'    TO COBR-JOB        OF OCOR1181
                       MOVE 'GFCT1181'    TO COBR-PROGRAMA   OF OCOR1181
04                     MOVE 14       TO COBR-SITUACAO-EVENTO OF OCOR1181
                       WRITE REG-COBRANCA OF OCOR1181
                       PERFORM 1160-TESTAR-FS-OCOR1181
                       ADD 1              TO ACU-GRAV-OCOR1181
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

10         MOVE ZEROS                  TO WRK-RETORNO-F.

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
               MOVE REG-COBRANCA       OF ATUALIZA
                                       TO REG-COBRANCA     OF OCOR1181
               MOVE 'GFCT0518'         TO COBR-JOB         OF OCOR1181
               MOVE 'GFCT1181'         TO COBR-PROGRAMA    OF OCOR1181
               MOVE 'GFCTB092'         TO COBR-RECURSO     OF OCOR1181
               MOVE SQLCODE            TO WRK-SQLCODE
               MOVE WRK-SQLCODE        TO COBR-COD-RETORNO OF OCOR1181
04             MOVE 14             TO COBR-SITUACAO-EVENTO OF OCOR1181
               MOVE 'ERRO NO SELECT DA TABELA'
                                       TO COBR-DESCRICAO   OF OCOR1181
               WRITE REG-COBRANCA      OF OCOR1181
               PERFORM 1160-TESTAR-FS-OCOR1181
               ADD 1                   TO ACU-GRAV-OCOR1181
           ELSE
               MOVE DEFETV-DEB-MOVTO   OF GFCTB092
                                       TO WRK-DATA-DEB
10                                        WRK-DEFETV-DEB-MOVTO
               MOVE DAGNDA-DEB-MOVTO   OF GFCTB092
                                       TO WRK-DATA-AGENDAMENTO
               MOVE VTARIF-DEB-MOVTO   OF GFCTB092
                                       TO WRK-VLR-DETALHES
AP1203         MOVE CSERVC-TARIF   OF GFCTB092
AP1203                                 TO WRK-CSERVC-TARIF
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
               PERFORM 3124-GRAVAR-FITA4
03             IF COBR-VALOR-DEBITAR   OF DETALHES
03                                 NOT EQUAL
03                VTARIF-DEB-MOVTO     OF GFCTB092
03                PERFORM 3133-GRAVAR-COB-PARCIAL
03             END-IF
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

10    *---> ATUALIZA SITUACAO DO EVENTO
10         IF   WRK-RETORNO-F        = ZEROS
10              MOVE WRK-CSIT-EVNTO-5525
10                                  TO CSIT-EVNTO-RECBD OF GFCTB092
10         ELSE
10              MOVE WRK-RETORNO-F
10                                  TO CSIT-EVNTO-RECBD OF GFCTB092
10         END-IF.

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

071205***** INICIO ****** UPDATE SUBSTITUIDO POR ARQUIVO *************
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
      *            QTENTV_DEB_MOVTO   = :GFCTB092.QTENTV-DEB-MOVTO,
05    *            CSIT_EVNTO_RECBD   = :GFCTB092.CSIT-EVNTO-RECBD
      *      WHERE
      *            DENVIO_MOVTO_TARIF = :GFCTB092.DENVIO-MOVTO-TARIF AND
      *            CROTNA_ORIGE_MOVTO = :GFCTB092.CROTNA-ORIGE-MOVTO AND
      *            CNRO_ARQ_MOVTO     = :GFCTB092.CNRO-ARQ-MOVTO     AND
      *            CSEQ_MOVTO         = :GFCTB092.CSEQ-MOVTO
      *    END-EXEC.
      *
      *    IF (SQLCODE                 NOT EQUAL ZEROS) OR
      *       (SQLWARN0                EQUAL 'W'      )
      *        MOVE REG-COBRANCA       OF ATUALIZA
      *                                TO REG-COBRANCA     OF OCOR1181
      *        MOVE 'GFCT0518'         TO COBR-JOB         OF OCOR1181
      *        MOVE 'GFCT1181'         TO COBR-PROGRAMA    OF OCOR1181
      *        MOVE 'GFCTB092'         TO COBR-RECURSO     OF OCOR1181
      *        MOVE SQLCODE            TO WRK-SQLCODE
04    *        MOVE 14             TO COBR-SITUACAO-EVENTO OF OCOR1181
      *        MOVE WRK-SQLCODE        TO COBR-COD-RETORNO OF OCOR1181
      *        MOVE 'ERRO NO UPDATE DA TABELA'
      *                                TO COBR-DESCRICAO   OF OCOR1181
      *        WRITE REG-COBRANCA      OF OCOR1181
      *        PERFORM 1160-TESTAR-FS-OCOR1181
      *        ADD 1                   TO ACU-GRAV-OCOR1181
      *    ELSE
      *        ADD 1                   TO ACU-COMMIT
      *        IF  ACU-COMMIT          EQUAL 5000000
      *            PERFORM 3122-EFETUAR-COMMIT
      *            MOVE ZEROS          TO ACU-COMMIT
      *        END-IF
      *    END-IF.

071205     MOVE DENVIO-MOVTO-TARIF TO GFCT67-DENVIO-MOVTO-TARIF
071205     MOVE CROTNA-ORIGE-MOVTO TO GFCT67-CROTNA-ORIGE-MOVTO
071205     MOVE CNRO-ARQ-MOVTO     TO GFCT67-CNRO-ARQ-MOVTO
071205     MOVE CSEQ-MOVTO         TO GFCT67-CSEQ-MOVTO
071205     MOVE CAG-DSTNO-MOVTO    TO GFCT67-CAG-DSTNO-MOVTO
071205     MOVE CCTA-DSTNO-MOVTO   TO GFCT67-CCTA-DSTNO-MOVTO
071205     MOVE CAG-CTLZA-DEB      TO GFCT67-CAG-CTLZA-DEB
071205     MOVE CCTA-CTLZA-DEB     TO GFCT67-CCTA-CTLZA-DEB
071205     MOVE QTENTV-DEB-MOVTO   TO GFCT67-QTENTV-DEB-MOVTO
071205     MOVE DAGNDA-DEB-MOVTO   TO GFCT67-DAGNDA-DEB-MOVTO
071205     MOVE VTARIF-DEB-MOVTO   TO GFCT67-VTARIF-DEB-MOVTO
071205     MOVE DEFETV-DEB-MOVTO   TO GFCT67-DEFETV-DEB-MOVTO
071205     MOVE CSIT-EVNTO-RECBD   TO GFCT67-CSIT-EVNTO-RECBD.

BI1210     MOVE COBR-NUM-PARC      OF DETALHES
BI1210                             TO GFCT67-NUM-PARC.

071205     WRITE GFCT67-GFCTB092.
071205
071205     PERFORM 1192-TESTAR-FS-ARQATU92.
071205
071205     ADD 1                       TO ACU-GRAV-ARQATU92.

071205**** FIM ********** UPDATE SUBSTITUIDO POR ARQUIVO *************

071204     PERFORM 3212-GRAVAR-ARQCOBR2.

      *----------------------------------------------------------------*
       3121-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

071205******************************************************************
071205*ROTINA PARA EFETUAR COMMIT                                      *
071205******************************************************************
071205*----------------------------------------------------------------*
071205*3122-EFETUAR-COMMIT             SECTION.
071205*----------------------------------------------------------------*
071205*
071205*    EXEC SQL
071205*        COMMIT
071205*    END-EXEC.
071205*
071205*    IF  SQLCODE                 NOT EQUAL ZEROS
071205*        MOVE 'DB2'              TO ERR-TIPO-ACESSO
071205*        MOVE 'COMMIT'           TO ERR-DBD-TAB
071205*                                   ERR-FUN-COMANDO
071205*        MOVE  SQLCODE           TO ERR-SQL-CODE
071205*        MOVE '0010'             TO ERR-LOCAL
071205*        MOVE  SPACES            TO ERR-SEGM
071205*        PERFORM 9999-PROCESSAR-ROTINA-ERRO
071205*    END-IF.
071205*
071205*----------------------------------------------------------------*
071205*3122-99-FIM.                    EXIT.
071205*----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA ATUALIZAR SITUACAO EVENTO                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3123-ATUALIZAR-SIT-EVENTO       SECTION.
      *----------------------------------------------------------------*

05         MOVE DENVIO-MOVTO-TARIF     OF GFCTB092
05                                     TO ZD-DENVIO-MOVTO-TARIF.
05         MOVE CROTNA-ORIGE-MOVTO     OF GFCTB092
05                                     TO ZD-CROTNA-ORIGE-MOVTO.
05         MOVE CNRO-ARQ-MOVTO         OF GFCTB092
05                                     TO ZD-CNRO-ARQ-MOVTO.
05         MOVE CSEQ-MOVTO             OF GFCTB092
05                                     TO ZD-CSEQ-MOVTO.
05         MOVE WRK-TIMESTAMP-H4       TO ZD-HTROCA-SIT-EVNTO.

           MOVE WRK-TIMESTAMP-H4       TO WRK-CPTCAO-GFCTB0H4
           MOVE WRK-CPTCAO-B0H4        TO ZD-CPTCAO-TBELA

05         MOVE WRK-CSIT-EVNTO-5525    TO ZD-CSIT-EVNTO-RECBD.
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

           IF   WRK-VLR-COBRADO-EVENTO  = ZEROS
                GO                     TO 3124-99-FIM.

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
AP1203     MOVE WRK-CSERVC-TARIF
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

11         PERFORM 3232-GRAVAR-ARQDEBIT.

      *----------------------------------------------------------------*
       3124-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA EVENTO COBRADO PARCIAL                              *
      ******************************************************************
      *----------------------------------------------------------------*
       3130-EVENTO-COBRADO-PARCIAL     SECTION.
      *----------------------------------------------------------------*

           IF  QTENTV-DEB-MOVTO        OF GFCTB092
01                               NOT LESS QMAX-TENTV-DEB OF GFCTB092
               PERFORM 3131-FIM-REPIQUE
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

02         IF VTARIF-DEB-MOVTO OF GFCTB092
02                                        = ZEROS         AND
02            WRK-VLR-ATUALIZA            = ZEROS
02            MOVE 12                    TO WRK-CSIT-EVNTO-5525
02         ELSE
02            MOVE 04                    TO WRK-CSIT-EVNTO-5525
02         END-IF.

           IF (COBR-DEB-PARCIAL          OF DETALHES
                                          = 1  AND
               WRK-VLR-ATUALIZA           > ZEROS)
                                                   OR
              (COBR-PARCIAL-ULT-REPIQ    OF DETALHES
                                          = 1  AND
               WRK-VLR-ATUALIZA           > ZEROS)
               COMPUTE WRK-VLR-DETALHES   = WRK-VLR-ATUALIZA +
                                            VTARIF-DEB-MOVTO OF GFCTB092
               MOVE ZEROS                TO WRK-VLR-ATUALIZA
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
                   PERFORM 3124-GRAVAR-FITA4
03                 PERFORM 3133-GRAVAR-COB-PARCIAL
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

      *----------------->> FIM DO REPIQUE POR VALOR MINIMO
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
                   ADD   1             TO QTENTV-DEB-MOVTO
                                       OF GFCTB092
                   MOVE 'S'            TO WRK-GRAVA-CONTABIL
      *            DISPLAY 'ENCERROU REPIQUE POR VALOR MINIMO'
                   GO                  TO 3132-CONTINUA
               END-IF
           END-IF.

      *----------------->> DEBITO PARCIAL
           IF  COBR-DEB-PARCIAL            OF DETALHES
                                           EQUAL 1
               COMPUTE WRK-VLR-DETALHES    = WRK-VLR-ATUALIZA +
                                           VTARIF-DEB-MOVTO OF GFCTB092
               MOVE 03                     TO WRK-CSIT-EVNTO-5525
               ADD   1                     TO QTENTV-DEB-MOVTO
                                              OF GFCTB092
               MOVE ZEROS                  TO WRK-VLR-ATUALIZA
               MOVE 'S'                    TO WRK-GRAVA-CONTABIL
      *        DISPLAY 'REPIQUE COM COBRANCA PARCIAL'
           ELSE
               MOVE 03                     TO WRK-CSIT-EVNTO-5525
               ADD   1                     TO QTENTV-DEB-MOVTO
                                              OF GFCTB092
               MOVE 'N'                    TO WRK-GRAVA-CONTABIL
      *        DISPLAY 'REPIQUE SEM COBRANCA PARCIAL'
           END-IF.

       3132-CONTINUA.

           PERFORM 3121-ATUALIZA-GFCTB092.

           IF  SQLCODE                     EQUAL ZEROS
070614*        PERFORM 3123-ATUALIZAR-SIT-EVENTO
070703         IF (COBR-VALOR-DEBITADO     OF ATUALIZA
070703                                     GREATER ZEROS AND
                   WRK-GRAVA-CONTABIL      EQUAL 'S'     AND
                   RETURN-CODE             EQUAL ZEROS   AND
                   WRK-5525-COD-RETORNO    EQUAL ZEROS)
070703             PERFORM 3123-ATUALIZAR-SIT-EVENTO
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

071205*---INIC--> SUBSTITUIR INSERT NA H4 POR GRAVACAO DE ARQUIVO <-----
071205     IF   WRK-VLR-COBRADO-EVENTO  = ZEROS
071205          GO                     TO 3133-99-FIM.
071205
071205*---INIC--> SUBSTITUIR INSERT NA H4 POR GRAVACAO DE ARQUIVO <-----
071205     MOVE COBR-DATA-ORIG         OF DETALHES
071205                                 TO GFCT68-DENVIO-MOVTO-TARIF
071205     MOVE COBR-ROTINA-ORIG       OF DETALHES
071205                                 TO GFCT68-CROTNA-ORIGE-MOVTO
071205     MOVE COBR-NUM-ARQ-ORIG      OF DETALHES
071205                                 TO GFCT68-CNRO-ARQ-MOVTO
071205     MOVE COBR-SEQ-ARQ-ORIG      OF DETALHES
071205                                 TO GFCT68-CSEQ-MOVTO
071205     MOVE WRK-VLR-COBRADO-EVENTO
071205                                 TO GFCT68-VDEB-PCIAL-TARIF
071205     MOVE GFCT06-DPROCM-ANTER    TO GFCT68-DDEB-PCIAL-TARIF
071205
071205     WRITE GFCT68-GFCTB0H7.
071205
071205     PERFORM 1194-TESTAR-FS-ARQATUH7.
071205
071205     ADD 1                       TO ACU-GRAV-ARQATUH7.

071205*    EXEC SQL
071205*      INSERT INTO DB2PRD.TDEB_PCIAL_TARIF
071205*           (DENVIO_MOVTO_TARIF,
071205*            CROTNA_ORIGE_MOVTO,
071205*            CNRO_ARQ_MOVTO,
071205*            CSEQ_MOVTO,
071205*            DDEB_PCIAL_TARIF,
071205*            VDEB_PCIAL_TARIF)
071205*        VALUES
071205*           (:GFCTB0H7.DENVIO-MOVTO-TARIF,
071205*            :GFCTB0H7.CROTNA-ORIGE-MOVTO,
071205*            :GFCTB0H7.CNRO-ARQ-MOVTO,
071205*            :GFCTB0H7.CSEQ-MOVTO,
071205*            :GFCTB0H7.DDEB-PCIAL-TARIF,
071205*            :GFCTB0H7.VDEB-PCIAL-TARIF)
071205*    END-EXEC.
071205*
071205*    IF (SQLCODE                 NOT EQUAL ZEROS) OR
071205*       (SQLWARN0                EQUAL 'W'      )
071205*        MOVE REG-COBRANCA       OF DETALHES
071205*                                TO REG-COBRANCA     OF OCOR1181
071205*        MOVE 'GFCT0520'         TO COBR-JOB         OF OCOR1181
071205*        MOVE 'GFCT1181'         TO COBR-PROGRAMA    OF OCOR1181
071205*        MOVE 'GFCTB0H7'         TO COBR-RECURSO     OF OCOR1181
071205*        MOVE SQLCODE            TO WRK-SQLCODE
071205*        MOVE WRK-SQLCODE        TO COBR-COD-RETORNO OF OCOR1181
071205*        MOVE 'ERRO NO INSERT DA TABELA'
071205*                                TO COBR-DESCRICAO   OF OCOR1181
04    *        MOVE 14             TO COBR-SITUACAO-EVENTO OF OCOR1181
071205*        WRITE REG-COBRANCA      OF OCOR1181
071205*        PERFORM 1160-TESTAR-FS-OCOR1181
071205*        ADD 1                   TO ACU-GRAV-OCOR1181
071205*    END-IF.
071205*---FIM --> SUBSTITUIR INSERT NA H4 POR GRAVACAO DE ARQUIVO <-----

      *    DISPLAY 'GRAVA COBR PARCIAL  SQL '  SQLCODE.
      *    DISPLAY 'CSEQ-MOVTO       =' CSEQ-MOVTO OF GFCTB0H7
      *    DISPLAY 'DDEB-PCIAL-TARIF =' DDEB-PCIAL-TARIF
      *    DISPLAY 'VDEB-PCIAL-TARIF =' VDEB-PCIAL-TARIF.

      *----------------------------------------------------------------*
       3133-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA ATUALIZAR DATA COBRANCA                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3140-ATUALIZA-DATA-COBRANCA     SECTION.
      *----------------------------------------------------------------*

      *    MOVE GFCT06-DPROCM-ATUAL    TO WRK-DATA-AGENDAMENTO.
           MOVE VTARIF-DEB-MOVTO       OF GFCTB092
                                       TO WRK-VLR-DETALHES.

           PERFORM 3121-ATUALIZA-GFCTB092.

      *----------------------------------------------------------------*
       3140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

071204******************************************************************
071204* ROTINA PARA GRAVAR ARQCOBR2                                    *
071204******************************************************************
071204*----------------------------------------------------------------*
071204 3212-GRAVAR-ARQCOBR2            SECTION.
071204*----------------------------------------------------------------*
071204
030408*    MOVE REG-COBRANCA     OF ATUALIZA TO REG-GFCTMW.
030408     MOVE REG-COBRANCA     OF DETALHES TO REG-GFCTMW.
071204     MOVE QMAX-TENTV-DEB   OF GFCTB092 TO GFCTMW-TENTV-DEB-MAX.
071204     MOVE VTARIF-LIQ-MOVTO OF GFCTB092 TO GFCTMW-VALOR-LIQ92.
071204     MOVE QTENTV-DEB-MOVTO OF GFCTB092 TO GFCTMW-TENTV-DEB-ATU.
071204     MOVE VTARIF-DEB-MOVTO OF GFCTB092 TO GFCTMW-VALOR-COBR92.
071204     MOVE CSIT-EVNTO-RECBD OF GFCTB092 TO GFCTMW-SITUACAO-EVENTO.
AP1203     MOVE WRK-CSERVC-TARIF-RS          TO GFCTMW-TARIFA-ORIG.


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


071204     WRITE REG-GFCTMW.
071204
071204     PERFORM 1190-TESTAR-FS-ARQCOBR2.
071204
071204     ADD 1                       TO ACU-GRAV-ARQCOBR2.
071204
071204*----------------------------------------------------------------*
071204 3212-99-FIM.                    EXIT.
071204*----------------------------------------------------------------*


11    ******************************************************************
11    * ROTINA PARA GRAVAR ARQUIVO DE EVENTOS EFETIVAMENTE COBRADOS    *
11    ******************************************************************
11    *----------------------------------------------------------------*
11     3232-GRAVAR-ARQDEBIT            SECTION.
11    *----------------------------------------------------------------*
11
11         MOVE COBR-AGENCIA-DEB       OF DETALHES
11                                     TO GFCTZJ-COD-AGENCIA.
11         MOVE COBR-CONTA-DEB         OF DETALHES
11                                     TO GFCTZJ-NUM-CONTA.
11         MOVE COBR-GRUPO-DEB         OF DETALHES
11                                     TO GFCTZJ-COD-GRUPO.
11         MOVE COBR-SUBGRUPO-DEB      OF DETALHES
11                                     TO GFCTZJ-COD-SUB-GRUPO.
11         MOVE COBR-CGC-CPF-MUNERO    OF DETALHES
11                                     TO GFCTZJ-CNPJ-CPF-PRINCIPAL.
11         MOVE COBR-CGC-CPF-FILIAL    OF DETALHES
11                                     TO GFCTZJ-CNPJ-CPF-FILIAL.
11         MOVE COBR-CGC-CPF-CONTROLE  OF DETALHES
11                                     TO GFCTZJ-CNPJ-CPF-CONTROLE.
AP1203     MOVE WRK-CSERVC-TARIF-RS
11                                     TO GFCTZJ-COD-EVENTO-TARIFA.
11         MOVE DEFETV-DEB-MOVTO       OF GFCTB092
11                                     TO WRK-DATA-DEB.
11         MOVE WRK-DIA-DEB            TO GFCTZJ-DIA-DEBITO.
11         MOVE WRK-MES-DEB            TO GFCTZJ-MES-DEBITO.
11         MOVE WRK-ANO-DEB            TO GFCTZJ-ANO-DEBITO.
11         MOVE ZEROS                  TO GFCTZJ-COD-IDENT-COBRANCA.
11         MOVE WRK-VLR-COBRADO-EVENTO TO GFCTZJ-VLR-COBRADO.

12         MOVE DOCOR-EVNTO            OF GFCTB092
12                                     TO WRK-DATA-EVT.
12         MOVE WRK-DIA-EVT            TO GFCTZJ-DIA-EVENTO.
12         MOVE WRK-MES-EVT            TO GFCTZJ-MES-EVENTO.
12         MOVE WRK-ANO-EVT            TO GFCTZJ-ANO-EVENTO.

11         IF   COBR-COD-RETORNO       OF ATUALIZA
11                                     IS NUMERIC
11              MOVE 1                 TO GFCTZJ-COD-IDENT-ORIG-DEB
11         ELSE
11              MOVE 2                 TO GFCTZJ-COD-IDENT-ORIG-DEB.
11
11         WRITE GFCTZJ-REGISTRO.
11
11         PERFORM 1180-TESTAR-FS-ARQDEBIT.
11
11         ADD 1                       TO ACU-GRAV-ARQDEBIT.
11
11    *----------------------------------------------------------------*
11     3232-99-FIM.                    EXIT.
11    *----------------------------------------------------------------*

10    ******************************************************************
10    * ROTINA PARA NAO ATUALIZAR SIT EVENTO NO FERIADO AGENCIA POUP   *
10    ******************************************************************
10    *----------------------------------------------------------------*
10     3400-RETORNAR-SIT-EVENTO        SECTION.
10    *----------------------------------------------------------------*
10         MOVE WRK-DEFETV-DEB-MOVTO      TO  WRK-DATA-DEB.
10
           IF  COBR-COD-RETORNO   OF ATUALIZA EQUAL '009'
               MOVE 3                    TO  WRK-RETORNO-F
           ELSE
10            IF   WRK-DEFETV-DEB-MOVTO       = '01.01.0001'
10                 MOVE 2                    TO  WRK-RETORNO-F
10    *                                       WRK-CSIT-EVNTO-5525
10            ELSE
10                 MOVE 3                    TO  WRK-RETORNO-F
10    *                                       WRK-CSIT-EVNTO-5525
              END-IF
10         END-IF.
10
10         PERFORM 3121-ATUALIZA-GFCTB092.
10    *    PERFORM 3123-ATUALIZAR-SIT-EVENTO.
10         ADD     1                      TO  ACU-FERIADO-AGEN.
10
10    *----------------------------------------------------------------*
10     3400-99-FIM.                    EXIT.
10    *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA PROCEDIMENTOS FINAIS                                *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

071205*    IF  ACU-COMMIT              GREATER ZEROS
071205*        PERFORM 3122-EFETUAR-COMMIT
071205*    END-IF.

           PERFORM 4100-EMITIR-DISPLAY.

           CLOSE ATUALIZA
                 DETALHES
                 CTRLDATA
                 SAIDETAL
                 FITCONTB
                 OCOR1181
                 ATLZB0H4
11               ARQDEBIT
071204           ARQCOBR2
071205           ARQATU92
071205           ARQATUH7.

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

           DISPLAY '******************** GFCT1181 ********************'.
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

           MOVE ACU-GRAV-OCOR1181      TO WRK-MASCARA.

           DISPLAY '* REGISTROS GRAVADOS EM OCOR1181: ' WRK-MASCARA
                                                                   ' *'.

05         MOVE ACU-GRAV-ATLZB0H4      TO WRK-MASCARA.
05
05         DISPLAY '* REGISTROS GRAVADOS EM GFCTB0H4: ' WRK-MASCARA
05                                                                 ' *'.

           MOVE ACU-DESPREZADOS        TO WRK-MASCARA.

           DISPLAY '* REGISTROS DESPREZADOS.........: ' WRK-MASCARA
                                                                   ' *'.

071205     MOVE ACU-GRAV-ARQATU92      TO WRK-MASCARA.

071205     DISPLAY '* REGISTROS A ATUALIZAR GFCTB092: ' WRK-MASCARA
                                                                   ' *'.
071205     MOVE ACU-GRAV-ARQATUH7      TO WRK-MASCARA.

071205     DISPLAY '* REGISTROS A ATUALIZAR GFCTB0H7: ' WRK-MASCARA
071205                                                             ' *'.

10         MOVE ACU-FERIADO-AGEN       TO WRK-MASCARA.
10
10         DISPLAY '* FERIADO NA AGENCIA POUP       : ' WRK-MASCARA
10                                                                 ' *'.

11         MOVE ACU-GRAV-ARQDEBIT      TO WRK-MASCARA.
11
11         DISPLAY '* REGISTROS GRAVADOS EM ARQDEBIT: ' WRK-MASCARA
11                                                                 ' *'.

071204     MOVE ACU-GRAV-ARQCOBR2      TO WRK-MASCARA.
071204
071204     DISPLAY '* REGISTROS GRAVADOS EM ARQCOBR2: ' WRK-MASCARA
071204                                                             ' *'.

           MOVE ACU-NF-DETALHES        TO WRK-MASCARA.

           DISPLAY '* REG. AGRUPADOS SEM DETALHES...: ' WRK-MASCARA
                                                                   ' *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT1181 ********************'.

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
           MOVE 'GFCT1181'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
