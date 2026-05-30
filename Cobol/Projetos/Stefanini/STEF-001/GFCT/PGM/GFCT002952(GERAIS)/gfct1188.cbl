      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1188.
       AUTHOR.     ALISON JUNIOR GHEDIN.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1188                                    *
      *    PROGRAMADOR.:   ALISON JUNIOR GHEDIN    - CPM PATO BRANCO   *
      *    ANALISTA CPM:   JOSSIANE CARLA GAZZONI  - CPM PATO BRANCO   *
      *    ANALISTA....:   WAGNER SILVA            - PROCKWORK GP. 50  *
      *    DATA........:   20/02/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   ATUALIZAR  REGISTROS F ZJ-VLR - COBRANCA    *
      *                    INDIVIDUAL.                                 *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                    ATLZINDV                  I#GFCTIL          *
      *                    CTRLDATA                  I#GFCT06          *
      *                    FITCONTB                  I#GFCTJH          *
      *                    OCOR1188                  I#GFCTIL          *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                TABLE                       INCLUDE/BOOK        *
      *                    DB2PRD.MOVTO_EVNTO_CRRTT  GFCTB092          *
      *                    DB2PRD.TDEB_PCIAL_TARIF   GFCTB0H7          *
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
      *    DATA........:   22/11/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   INIBIR ATUALIZACAO DO CPO DAGNDA-DEB-MOVTO  *
      *----------------------------------------------------------------*
      *================================================================*
      *    PROGRAMA....:   ALTERACAO                                   *
      *    ANALISTA....:   WAGNER SILVA            - PROCKWORK GP. 50  *
      *    DATA........:   12/01/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   ATUALIZAR AG/CTA DEBITO OU CENTRALIZADORA,  *
      *                    EM CASO DE TRAG NO MORA/POUP                *
      *----------------------------------------------------------------*
      *================================================================*
      *    PROGRAMA....:   ALTERACAO                                   *
      *    ANALISTA....:   WAGNER SILVA            - PROCKWORK GP. 50  *
      *    DATA........:   21/03/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   INCLUIR TRATAMENTO PARA COD-RETORNO 001 E   *
      *                    012         (LOCAIS ALT. MARCADOS C/ 0321)  *
      *----------------------------------------------------------------*
      *================================================================*
      *    PROGRAMA....:   ALTERACAO                                   *
      *    ANALISTA....:   WAGNER SILVA            - PROCKWORK GP. 50  *
      *    DATA........:   22/03/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:                                               *
      *          SE COBR-COD-RETORNO = 001 OU 012:                     *
      *             CSIT_EVNTO_RECBD(GFCTB092) = 03                    *
      *             VTARIF_DEB_MOVT(GFCTB092)  = NAO ATUALIZAR O VALOR *
      *             NAO GERAR REGISTRO NA FITA CONTABIL (FITCONTB)     *
      *             NAO GERAR REGISTRO DE DEBITO PARCIAL (GFCTB0H7)    *
      *================================================================*
      *    PROGRAMA....:   ALTERACAO                                   *
      *    ANALISTA....:   WAGNER SILVA            - PROCKWORK GP. 50  *
      *    DATA........:   23/03/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:                                               *
      *          SE COBRANCA TOTAL                                     *
      *             VERIFICAR SE VALOR COBRADO DIFERENTE DE VALOR      *
      *             LIQUIDO(GFCTB092)                                  *
      *             SE POSITIVO GRAVAR GFCTB0H7, DEBITO PARCIAL        *
      *                (REPIQUE ENCERRADO)                             *
      *================================================================*
2703  *================================================================*
2703  *=                      A L T E R A C A O                       =*
2703  *================================================================*
2703  *    ANALISTA....:   WAGNER SILVA            - PROCKWORK GP. 50  *
2703  *    DATA........:   27/03/2007                                  *
2703  *----------------------------------------------------------------*
2703  *    OBJETIVO....:   RETIRAR ACESSO AO MODULO GFCT5525           *
2703  *                    ALTERAR SITUACAO DO EVENTO NO UPDATE DA 92  *
2703  *                    GRAVAR ARQUIVO P/ GRAVAR H4 POSTERIORMENTE  *
2703  *----------------------------------------------------------------*
0410  *================================================================*
0410  *=                      A L T E R A C A O                       =*
0410  *================================================================*
0410  *    ANALISTA....:   WAGNER SILVA            - PROCKWORK GP. 50  *
0410  *    DATA........:   10/04/2007                                  *
0410  *----------------------------------------------------------------*
0410  *    OBJETIVO....:   GRAVAR OCORRENCIA SE VALOR RETORNADO DO MORA*
0410  *                    MAIOR QUE VALOR ENVIADO                     *
0410  *----------------------------------------------------------------*
070510*================================================================*
070510*=                      A L T E R A C A O                       =*
070510*================================================================*
070510*    ANALISTA....:   WAGNER SILVA            - PROCKWORK GP. 50  *
070510*    DATA........:   10/05/2007                                  *
070510*----------------------------------------------------------------*
070510*    OBJETIVO....:   INCLUIR TRATAMENTO PARA COBR-COD-RETORNO:F  *
070510*                    RETORNAR SITUACAO EVENTO DA 92 P/ SITUACAO  *
070510*                    ANTERIOR A COBRANCA                         *
070510*----------------------------------------------------------------*
070511*================================================================*
070511*=                      A L T E R A C A O                       =*
070511*================================================================*
070511*    ANALISTA....:   WAGNER SILVA            - PROCKWORK GP. 50  *
070511*    DATA........:   11/05/2007                                  *
070511*----------------------------------------------------------------*
070511*    OBJETIVO....:   INCLUIR ARQUIVO CONTENDO INFORMACOES SOBRE  *
070511*                    REGISTROS COBRADOS                          *
070511*----------------------------------------------------------------*
070604*================================================================*
070604*=                      A L T E R A C A O                       =*
070604*================================================================*
070604*    ANALISTA....:   WAGNER SILVA            - PROCKWORK GP. 50  *
070604*    DATA........:   04/06/2007                                  *
070604*----------------------------------------------------------------*
070604*    OBJETIVO....:   INCLUIR DATA DO EVENTO NO ARQUIVO ARQDEBIT  *
070604*----------------------------------------------------------------*
070612*================================================================*
070612*    ANALISTA....:   WAGNER SILVA            - PROCKWORK GP. 50  *
070612*    DATA........:   12/06/2007                                  *
070612*----------------------------------------------------------------*
070612*    OBJETIVO....:   DEIXAR DE GRAVAR GFCTB0H4 EM CASO DE REPIQUE*
070612*----------------------------------------------------------------*
070920*================================================================*
070920*=                      A L T E R A C A O                       =*
070920*================================================================*
070920*    ANALISTA....:   WAGNER SILVA            - PROCKWORK GP. 50  *
070920*    DATA........:   20/09/2007                                  *
070920*----------------------------------------------------------------*
070920*    OBJETIVO....:   INCLUIR ARQUIVO CONTENDO INFORMACOES SOBRE  *
070920*                    REGISTROS COBRADOS                          *
070920*================================================================*
071101*================================================================*
071101*=                      A L T E R A C A O                       =*
071101*================================================================*
071101*   ANALISTA....:   WAGNER SILVA            - PROCKWORK GP. 50   *
071101*   DATA........:   01/11/2007                                   *
071101*----------------------------------------------------------------*
071101*   OBJETIVOS...:                                                *
071101*    SUBSTITUIR UPDATE DA TABELA GFCTBO92 POR GERACAO DE ARQUIVO *
071101*    SUBSTITUIR INSERT DA TABELA GFCTBOH7 POR GERACAO DE ARQUIVO *
071101*================================================================*
      *                                                                *
      *                           ALTERACAO                            *
      *                           ---------                            *
      *                                                                *
      * RESPONSAVEL: FLAVIO ALVES GERADE - GP 08 - PERFORMANCE.        *
      * DATA       : 15/04/2010.                                       *
      * OBJETIVO   : MELHORIA DE PERFORMANCE NO ACESSO A TABELA        *
      *              GFCTB092 E NA LOGICA DO PROGRAMA.                 *
      *                                                                *
      ******************************************************************
BI1210*=                      A L T E R A C A O                       =*
BI1210*================================================================*
BI1210*   ANALISTA....:   UBIRAJARA (BIRA) - PROCWORK GP. 50           *
BI1210*   DATA........:   20/12/2010                                   *
BI1210*----------------------------------------------------------------*
BI1210*   OBJETIVOS...:                                                *
BI1210*    MOVIMENTAR O CAMPO (ATLZINDV) DE (DETALHES) PARA            *
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

           SELECT ATLZINDV  ASSIGN     TO UT-S-ATLZINDV
           FILE STATUS                 IS WRK-FS-ATLZINDV.

           SELECT CTRLDATA ASSIGN      TO UT-S-CTRLDATA
           FILE STATUS                 IS WRK-FS-CTRLDATA.

           SELECT FITCONTB ASSIGN      TO UT-S-FITCONTB
           FILE STATUS                 IS WRK-FS-FITCONTB.

           SELECT OCOR1188 ASSIGN      TO UT-S-OCOR1188
           FILE STATUS                 IS WRK-FS-OCOR1188.

2703       SELECT ATLZB0H4  ASSIGN     TO UT-S-ATLZB0H4
2703       FILE STATUS                 IS WRK-FS-ATLZB0H4.

070511     SELECT ARQDEBIT  ASSIGN     TO UT-S-ARQDEBIT
070511     FILE STATUS                 IS WRK-FS-ARQDEBIT.

070920     SELECT ARQCOBR2  ASSIGN     TO UT-S-ARQCOBR2
070920     FILE STATUS                 IS WRK-FS-ARQCOBR2.

071101     SELECT ARQATU92  ASSIGN     TO UT-S-ARQATU92
071101     FILE STATUS                 IS WRK-FS-ARQATU92.

071101     SELECT ARQATUH7  ASSIGN     TO UT-S-ARQATUH7
071101     FILE STATUS                 IS WRK-FS-ARQATUH7.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  REGISTROS INDIVIDUAIS COBRADOS PARA ATUALIZACAO NA  *
      *            TAB. GFCTB092.                                      *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  ATLZINDV
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
      *    OUTPUT: ARQ. P/ GERACAO DA FITA4                            *
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

       FD  OCOR1188
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       COPY 'I#GFCTIL'.

2703  *----------------------------------------------------------------*
2703  *   OUTPUT:  REGISTROS PARA ATUALIZACAO DA TABELA GFCTB0H4       *
2703  *            ORG. SEQUENCIAL     -   LRECL   =   050             *
2703  *----------------------------------------------------------------*
2703
2703   FD  ATLZB0H4
2703       RECORDING MODE IS F
2703       LABEL RECORD IS STANDARD
2703       BLOCK CONTAINS  0 RECORDS.
2703
2703   COPY 'I#GFCTZD'.

070511*----------------------------------------------------------------*
070511*   OUTPUT:  REGISTROS EFETIVAMENTE DEBITADOS                    *
070511*            ORG. SEQUENCIAL     -   LRECL   =   060             *
070511*----------------------------------------------------------------*
070511
070511 FD  ARQDEBIT
070511     RECORDING MODE IS F
070511     LABEL RECORD IS STANDARD
070511     BLOCK CONTAINS  0 RECORDS.
070511
070511 COPY 'I#GFCTZJ'.

070920*----------------------------------------------------------------*
070920*   OUTPUT:  REGISTROS EFETIVAMENTE DEBITADOS                    *
070920*            ORG. SEQUENCIAL     -   LRECL   =   340             *
070920*----------------------------------------------------------------*
070920
070920 FD  ARQCOBR2
070920     RECORDING MODE IS F
070920     LABEL RECORD IS STANDARD
070920     BLOCK CONTAINS  0 RECORDS.
070920
070920 COPY 'I#GFCTMW'.

071101*----------------------------------------------------------------*
071101*   OUTPUT:  REGISTROS PARA ATUALIZACAO NA TABELA GFCTB092       *
071101*            ORG. SEQUENCIAL     -   LRECL   =   090             *
071101*----------------------------------------------------------------*
071101
071101 FD  ARQATU92
071101     RECORDING MODE IS F
071101     LABEL RECORD IS STANDARD
071101     BLOCK CONTAINS  0 RECORDS.
071101
071101 COPY 'I#GFCT67'.

071101*----------------------------------------------------------------*
071101*   OUTPUT:  REGISTROS PARA INSERIR  NA TABELA GFCTB0H7          *
071101*            ORG. SEQUENCIAL     -   LRECL   =   038             *
071101*----------------------------------------------------------------*
071101
071101 FD  ARQATUH7
071101     RECORDING MODE IS F
071101     LABEL RECORD IS STANDARD
071101     BLOCK CONTAINS  0 RECORDS.
071101
071101 COPY 'I#GFCT68'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)          VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* ACUMULADORES *'.
      *----------------------------------------------------------------*

2703   77  ACU-GRAV-ATLZB0H4           PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-ATLZINDV          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-CTRLDATA          PIC  9(009) COMP-3  VALUE ZEROS.
070511 77  ACU-GRAV-ARQDEBIT           PIC  9(009) COMP-3  VALUE ZEROS.
070920 77  ACU-GRAV-ARQCOBR2           PIC  9(009) COMP-3  VALUE ZEROS.
071101 77  ACU-GRAV-ARQATU92           PIC  9(009) COMP-3  VALUE ZEROS.
071101 77  ACU-GRAV-ARQATUH7           PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAV-FITCONTB           PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAV-OCOR1188           PIC  9(009) COMP-3  VALUE ZEROS.
070510 77  ACU-FERIADO-AGEN            PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-DESPREZADOS             PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-VALOR-CONTABIL          PIC  9(11)V99 COMP-3 VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* TESTES DE FILE STATUS *'.
      *----------------------------------------------------------------*

070510 77  WRK-FS-ARQDEBIT             PIC  X(002)         VALUE SPACES.
070920 77  WRK-FS-ARQCOBR2             PIC  X(002)         VALUE SPACES.
071101 77  WRK-FS-ARQATU92             PIC  X(002)         VALUE SPACES.
071101 77  WRK-FS-ARQATUH7             PIC  X(002)         VALUE SPACES.
2703   77  WRK-FS-ATLZB0H4             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ATLZINDV             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-CTRLDATA             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-FITCONTB             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-OCOR1188             PIC  X(002)         VALUE SPACES.

0410   77  WRK-MSG-MAIOR               PIC  X(050)         VALUE
0410       ' MORA/POUP COBROU UM VALOR MAIOR QUE O SOLICITADO '.
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
           '* VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-MASCARA                 PIC  ZZ.ZZZ.ZZZ.ZZ9 VALUE ZEROS.
       77  WRK-MASCAR1                 PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
       77  WRK-SQLCODE                 PIC  9(003)         VALUE ZEROS.
       77  WRK-CSIT-EVENTO-5525        PIC  9(002)         VALUE ZEROS.
070510 77  WRK-RETORNO-F               PIC  9(002)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* VARIAVEIS AUXILIARES DE DATA *'.
      *----------------------------------------------------------------*

070510 01  WRK-DEFETV-DEB-MOVTO        PIC  X(010)         VALUE SPACES.

       01  WRK-DATA-AUX.
           03  WRK-ANO-AUX             PIC  9(004)         VALUE ZEROS.
           03  WRK-MES-AUX             PIC  9(002)         VALUE ZEROS.
           03  WRK-DIA-AUX             PIC  9(002)         VALUE ZEROS.

       01  WRK-DATA-DB2.
           03  WRK-DIA-DB2             PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-MES-DB2             PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-ANO-DB2             PIC  9(004)         VALUE ZEROS.

070511 01  WRK-DATA-DEB.
070511     03  WRK-DIA-DEB             PIC  9(002)         VALUE ZEROS.
070511     03  FILLER                  PIC  X(001)         VALUE '.'.
070511     03  WRK-MES-DEB             PIC  9(002)         VALUE ZEROS.
070511     03  FILLER                  PIC  X(001)         VALUE '.'.
070511     03  WRK-ANO-DEB             PIC  9(004)         VALUE ZEROS.

2703   01  WRK-TIMESTAMP-H4.
2703       03  WRK-DTIME.
2703           05  WRK-AAAA            PIC 9(04)    VALUE ZEROS.
2703           05  WRK-TRACO1          PIC X(01)    VALUE SPACES.
2703           05  WRK-MM              PIC 9(02)    VALUE ZEROS.
2703           05  WRK-TRACO2          PIC X(01)    VALUE SPACES.
2703           05  WRK-DD              PIC 9(02)    VALUE ZEROS.
2703       03  WRK-RESTO.
2703           05  WRK-TRACO3          PIC X(01)    VALUE SPACES.
2703           05  WRK-HR              PIC 9(02)    VALUE ZEROS.
2703           05  WRK-PONTO1          PIC X(01)    VALUE SPACES.
2703           05  WRK-MI              PIC 9(02)    VALUE ZEROS.
2703           05  WRK-PONTO2          PIC X(01)    VALUE SPACES.
2703           05  WRK-SG              PIC 9(02)    VALUE ZEROS.
2703           05  WRK-PONTO3          PIC X(01)    VALUE SPACES.
2703           05  WRK-DS              PIC 9(06)    VALUE ZEROS.
2703
2703   01  WRK-TIMESTAMP-SIST.
2703       05  WRK-AAAA-S              PIC 9(04)    VALUE ZEROS.
2703       05  WRK-MM-S                PIC 9(02)    VALUE ZEROS.
2703       05  WRK-DD-S                PIC 9(02)    VALUE ZEROS.
2703       05  WRK-HR-S                PIC 9(02)    VALUE ZEROS.
2703       05  WRK-MI-S                PIC 9(02)    VALUE ZEROS.
2703       05  WRK-SG-S                PIC 9(02)    VALUE ZEROS.
2703       05  WRK-DS-S                PIC 9(06)    VALUE ZEROS.
2703
2703   01  WRK-DT-R                    PIC 9(09)    VALUE ZEROS.
2703   01  WRK-DATA-AAAAMMDD.
2703       05  WRK-DATA-LIXO           PIC 9(01)    VALUE ZEROS.
2703       05  WRK-DATA-AAAA           PIC 9(04)    VALUE ZEROS.
2703       05  WRK-DATA-MM             PIC 9(02)    VALUE ZEROS.
2703       05  WRK-DATA-DD             PIC 9(02)    VALUE ZEROS.
2703
2703  *---------------------------------------------------------------*
2703   01  FILLER                      PIC  X(050)         VALUE
2703       '*** AREA DA POOL7600 ***'.
2703  *---------------------------------------------------------------*
2703
2703   01  WRK-DATA-HORA.
2703       03  WRK-DT-JULIANA          PIC  9(005) COMP-3  VALUE ZEROS.
2703       03  WRK-DT-AAMMDD           PIC  9(007) COMP-3  VALUE ZEROS.
2703       03  WRK-DT-AAAAMMDD         PIC  9(009) COMP-3  VALUE ZEROS.
2703       03  WRK-TI-HHMMSS           PIC  9(007) COMP-3  VALUE ZEROS.
2703       03  WRK-TI-HHMMSSMMMMMM     PIC  9(013) COMP-3  VALUE ZEROS.
2703       03  WRK-TIMESTAMP           PIC  X(020)         VALUE SPACES.


PCT    01 WRK-CPTCAO-GFCTB0H4          PIC  X(026) VALUE SPACES.
       01 FILLER                       REDEFINES WRK-CPTCAO-GFCTB0H4.
           05 FILLER                   PIC X(023).
PCT        05 WRK-CPTCAO-B0H4          PIC 9(003).


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE MENSAGEM DE ERRO *'.
      *----------------------------------------------------------------*

       01  WRK-MSG-FS.
           05  FILLER                  PIC  X(007)         VALUE SPACES.
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
           '* COMUNICACAO COM MODULO GFCT5525 *'.
      *----------------------------------------------------------------*

       01  FILLER.
           05  WRK-5525-COD-RETORNO    PIC  9(002)         VALUE ZEROS.

       COPY  'I#GFCT0M'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DA POOL7100 *'.
      *----------------------------------------------------------------*

       COPY POL7100C.

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

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      * CHAMA AS ROTINAS A SEREM EXECUTADAS.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-FS-ATLZINDV     EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EFETUAR ABERTURA DOS ARQUIVOS.                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           OPEN INPUT  ATLZINDV
                       CTRLDATA
                OUTPUT FITCONTB
                       OCOR1188
2703                   ATLZB0H4
070511                 ARQDEBIT
070920                 ARQCOBR2
071101                 ARQATU92
071101                 ARQATUH7.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

2703       PERFORM 1200-FORMATAR-TIMESTAMP-H4.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMAR TESTE DE FILE STATUS.                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-ATLZINDV.

           PERFORM 1120-TESTAR-FS-CTRLDATA.

           PERFORM 1130-TESTAR-FS-FITCONTB.

           PERFORM 1140-TESTAR-FS-OCOR1188.

2703       PERFORM 1150-TESTAR-FS-ATLZB0H4.

070511     PERFORM 1160-TESTAR-FS-ARQDEBIT.

070920     PERFORM 1170-TESTAR-FS-ARQCOBR2.

071101     PERFORM 1180-TESTAR-FS-ARQATU92.

071101     PERFORM 1190-TESTAR-FS-ARQATUH7.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TESTAR FILE STATUS DO ARQUIVO ATLZINDV                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-ATLZINDV         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ATLZINDV         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-ATLZINDV    TO WRK-FILE-STATUS
               MOVE 'ATLZINDV '        TO WRK-ARQUIVO
               MOVE WRK-MSG-FS         TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TESTAR FILE STATUS DO ARQUIVO CTRLDATA                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-CTRLDATA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CTRLDATA         NOT EQUAL '00'
               MOVE WRK-FS-CTRLDATA    TO WRK-FILE-STATUS
               MOVE 'CTRLDATA'         TO WRK-ARQUIVO
               MOVE WRK-MSG-FS         TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TESTAR FILE STATUS DO ARQUIVO FITCONTB                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1130-TESTAR-FS-FITCONTB         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-FITCONTB         NOT EQUAL '00'
               MOVE WRK-FS-FITCONTB    TO WRK-FILE-STATUS
               MOVE 'FITCONTB'         TO WRK-ARQUIVO
               MOVE WRK-MSG-FS         TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TESTAR FILE STATUS DO ARQUIVO OCOR1188                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1140-TESTAR-FS-OCOR1188         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-OCOR1188         NOT EQUAL '00'
               MOVE WRK-FS-OCOR1188    TO WRK-FILE-STATUS
               MOVE 'OCOR1188'         TO WRK-ARQUIVO
               MOVE WRK-MSG-FS         TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

2703  ******************************************************************
2703  * TESTAR FILE STATUS DO ARQUIVO ATLZB0H4                         *
2703  ******************************************************************
2703  *----------------------------------------------------------------*
2703   1150-TESTAR-FS-ATLZB0H4         SECTION.
2703  *----------------------------------------------------------------*
2703
2703       IF  WRK-FS-ATLZB0H4         NOT EQUAL '00'
2703           MOVE WRK-FS-ATLZB0H4    TO WRK-FILE-STATUS
2703           MOVE 'ATLZB0H4'         TO WRK-ARQUIVO
2703           MOVE WRK-MSG-FS         TO ERR-TEXTO
2703           MOVE 'APL'              TO ERR-TIPO-ACESSO
2703           PERFORM 9999-PROCESSAR-ROTINA-ERRO
2703       END-IF.
2703
2703  *----------------------------------------------------------------*
2703   1150-99-FIM.                    EXIT.
2703  *----------------------------------------------------------------*

070511******************************************************************
070511* TESTAR FILE STATUS DO ARQUIVO ARQDEBIT                         *
070511******************************************************************
070511*----------------------------------------------------------------*
070511 1160-TESTAR-FS-ARQDEBIT         SECTION.
070511*----------------------------------------------------------------*
070511
070511     IF  WRK-FS-ARQDEBIT         NOT EQUAL '00'
070511         MOVE WRK-FS-ARQDEBIT    TO WRK-FILE-STATUS
070511         MOVE 'ARQDEBIT'         TO WRK-ARQUIVO
070511         MOVE WRK-MSG-FS         TO ERR-TEXTO
070511         MOVE 'APL'              TO ERR-TIPO-ACESSO
070511         PERFORM 9999-PROCESSAR-ROTINA-ERRO
070511     END-IF.
070511
070511*----------------------------------------------------------------*
070511 1160-99-FIM.                    EXIT.
070511*----------------------------------------------------------------*

070920******************************************************************
070920* TESTAR FILE STATUS DO ARQUIVO ARQCOBR2                         *
070920******************************************************************
070920*----------------------------------------------------------------*
070920 1170-TESTAR-FS-ARQCOBR2         SECTION.
070920*----------------------------------------------------------------*
070920
070920     IF  WRK-FS-ARQCOBR2         NOT EQUAL '00'
070920         MOVE WRK-FS-ARQCOBR2    TO WRK-FILE-STATUS
070920         MOVE 'ARQCOBR2'         TO WRK-ARQUIVO
070920         MOVE WRK-MSG-FS         TO ERR-TEXTO
070920         MOVE 'APL'              TO ERR-TIPO-ACESSO
070920         PERFORM 9999-PROCESSAR-ROTINA-ERRO
070920     END-IF.
070920
070920*----------------------------------------------------------------*
070920 1170-99-FIM.                    EXIT.
070920*----------------------------------------------------------------*

071101******************************************************************
071101* TESTAR FILE STATUS DO ARQUIVO ARQATU92                         *
071101******************************************************************
071101*----------------------------------------------------------------*
071101 1180-TESTAR-FS-ARQATU92         SECTION.
071101*----------------------------------------------------------------*
071101
071101     IF  WRK-FS-ARQATU92         NOT EQUAL '00'
071101         MOVE WRK-FS-ARQATU92    TO WRK-FILE-STATUS
071101         MOVE 'ARQATU92'         TO WRK-ARQUIVO
071101         MOVE WRK-MSG-FS         TO ERR-TEXTO
071101         MOVE 'APL'              TO ERR-TIPO-ACESSO
071101         PERFORM 9999-PROCESSAR-ROTINA-ERRO
071101     END-IF.
071101
071101*----------------------------------------------------------------*
071101 1180-99-FIM.                    EXIT.
071101*----------------------------------------------------------------*

071101******************************************************************
071101* TESTAR FILE STATUS DO ARQUIVO ARQATUH7                         *
071101******************************************************************
071101*----------------------------------------------------------------*
071101 1190-TESTAR-FS-ARQATUH7         SECTION.
071101*----------------------------------------------------------------*
071101
071101     IF  WRK-FS-ARQATUH7         NOT EQUAL '00'
071101         MOVE WRK-FS-ARQATUH7    TO WRK-FILE-STATUS
071101         MOVE 'ARQATUH7'         TO WRK-ARQUIVO
071101         MOVE WRK-MSG-FS         TO ERR-TEXTO
071101         MOVE 'APL'              TO ERR-TIPO-ACESSO
071101         PERFORM 9999-PROCESSAR-ROTINA-ERRO
071101     END-IF.
071101
071101*----------------------------------------------------------------*
071101 1190-99-FIM.                    EXIT.
071101*----------------------------------------------------------------*

2703  *----------------------------------------------------------------*
2703   1200-FORMATAR-TIMESTAMP-H4     SECTION.
2703  *----------------------------------------------------------------*
2703
2703       CALL 'POOL7600'             USING WRK-DATA-HORA.
2703
2703       MOVE WRK-DT-AAAAMMDD        TO WRK-DT-R.
2703       MOVE WRK-DT-R               TO WRK-DATA-AAAAMMDD.
2703       MOVE WRK-DATA-DD            TO WRK-DD.
2703       MOVE WRK-DATA-MM            TO WRK-MM.
2703       MOVE WRK-DATA-AAAA          TO WRK-AAAA.
2703       MOVE '-'                    TO WRK-TRACO1 WRK-TRACO2.
2703
2703       MOVE  WRK-TIMESTAMP         TO WRK-TIMESTAMP-SIST.
2703       MOVE  WRK-HR-S              TO WRK-HR.
2703       MOVE  WRK-MI-S              TO WRK-MI.
2703       MOVE  WRK-SG-S              TO WRK-SG.
2703       MOVE  WRK-DS-S              TO WRK-DS.
2703       MOVE  '-'                   TO WRK-TRACO3.
2703       MOVE  '.'                   TO WRK-PONTO1
2703                                      WRK-PONTO2
2703                                      WRK-PONTO3.
2703
2703  *----------------------------------------------------------------*
2703   1200-99-FIM.                    EXIT.
2703  *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICA SE OS ARQUIVOS ESTAO VAZIOS                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-ATLZINDV.

           IF  WRK-FS-ATLZINDV         EQUAL '10'
               DISPLAY '***************** GFCT1188 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*          ARQUIVO ATLZINDV VAZIO          *'
               DISPLAY '*         PROCESSAMENTO  ENCERRADO         *'
               DISPLAY '*                                          *'
               DISPLAY '***************** GFCT1188 *****************'
               PERFORM 4000-FINALIZAR
           END-IF.

           PERFORM 2200-LER-CTRLDATA.

           IF  WRK-FS-CTRLDATA          EQUAL '10'
               DISPLAY '***************** GFCT1188 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*          ARQUIVO CTRLDATA VAZIO          *'
               DISPLAY '*         PROCESSAMENTO  ENCERRADO         *'
               DISPLAY '*                                          *'
               DISPLAY '***************** GFCT1188 *****************'
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EFETUA A LEITURA DO ARQUIVO ATLZINDV                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-ATLZINDV               SECTION.
      *----------------------------------------------------------------*

           READ ATLZINDV.

           IF  WRK-FS-ATLZINDV         EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-ATLZINDV.

           ADD 1                       TO ACU-LIDOS-ATLZINDV.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * EFETUA A LEITURA DO ARQUIVO CTRLDATA                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-LER-CTRLDATA               SECTION.
      *----------------------------------------------------------------*

           READ CTRLDATA.

           IF  WRK-FS-CTRLDATA         EQUAL '10'
               GO TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-CTRLDATA.

           ADD 1                       TO ACU-LIDOS-CTRLDATA.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA DE PROCESSAMENTO PRINCIPAL DO PROGRAMA                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

______*____IF__COBR-COD-RETORNO_OF___ATLZINDV_EQUAL_'009'
______*________ADD__1___________TO___ACU-DESPREZADOS
______*________PERFORM__2100-LER-ATLZINDV
______*________GO_______________TO___3000-99-FIM
______*____END-IF.

           PERFORM 3100-ACESSAR-GFCTB092.

           IF  SQLCODE                         NOT EQUAL ZEROS
           OR  CSIT-EVNTO-RECBD OF GFCTB092    NOT EQUAL 13
               ADD      1                      TO ACU-DESPREZADOS
               PERFORM  2100-LER-ATLZINDV
               GO               TO   3000-99-FIM
           END-IF.

           MOVE WRK-GRAVACAO                   TO WRK-OPERACAO.

           EVALUATE TRUE

               WHEN COBR-COD-RETORNO           OF ATLZINDV
                                               EQUAL '000' OR '010'
                                               OR 'P'      OR 'I'
                   IF  COBR-VALOR-DEBITAR      OF ATLZINDV
                                               EQUAL COBR-VALOR-DEBITADO
                                               OF ATLZINDV
                       PERFORM 3200-EVENTO-COBRADO-TOTAL
                   ELSE
                       IF  COBR-VALOR-DEBITAR  OF ATLZINDV
                                             GREATER COBR-VALOR-DEBITADO
                                               OF ATLZINDV
                           PERFORM 3300-EVENTO-COBRADO-PARCIAL
0401                   ELSE
0401                       MOVE REG-COBRANCA   OF ATLZINDV
0401                                           TO REG-COBRANCA
0401                                           OF OCOR1188
0401                       MOVE 14             TO COBR-SITUACAO-EVENTO
0401                                           OF OCOR1188
0401                       MOVE 'GFCT0516'     TO COBR-JOB
0401                                           OF OCOR1188
0401                       MOVE 'GFCT1188'     TO COBR-PROGRAMA
0401                                           OF OCOR1188
0401                       MOVE WRK-MSG-MAIOR  TO COBR-DESCRICAO
0401                                           OF OCOR1188
0401                       WRITE REG-COBRANCA  OF OCOR1188
0401                       PERFORM 1140-TESTAR-FS-OCOR1188
0401                       ADD 1               TO ACU-GRAV-OCOR1188
                       END-IF
                   END-IF

0321           WHEN COBR-COD-RETORNO           OF ATLZINDV
0321                                           EQUAL '001' OR '012'
0321                PERFORM 3300-EVENTO-COBRADO-PARCIAL

070710         WHEN COBR-COD-RETORNO           OF ATLZINDV
070510                                         EQUAL 'F' OR '009'
070510             PERFORM 3400-RETORNAR-SIT-EVENTO

               WHEN OTHER
                    MOVE  REG-COBRANCA     OF  ATLZINDV
                                           TO  REG-COBRANCA
                                           OF  OCOR1188
                    MOVE  14               TO  COBR-SITUACAO-EVENTO
                                           OF  OCOR1188
                    MOVE  'GFCT0516'       TO  COBR-JOB
                                           OF  OCOR1188
                    MOVE  'GFCT1188'       TO  COBR-PROGRAMA
                                           OF  OCOR1188
                    WRITE   REG-COBRANCA   OF  OCOR1188
                    PERFORM 1140-TESTAR-FS-OCOR1188
                    ADD     1              TO  ACU-GRAV-OCOR1188
           END-EVALUATE.

           PERFORM 2100-LER-ATLZINDV.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA DE ACESSO A TABELA GFCTB092                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-ACESSAR-GFCTB092           SECTION.
      *----------------------------------------------------------------*

           MOVE COBR-DATA-ORIG         OF ATLZINDV
                                       TO DENVIO-MOVTO-TARIF
                                                           OF GFCTB092.
           MOVE COBR-ROTINA-ORIG       OF ATLZINDV
                                       TO CROTNA-ORIGE-MOVTO
                                                           OF GFCTB092.
           MOVE COBR-NUM-ARQ-ORIG      OF ATLZINDV
                                       TO CNRO-ARQ-MOVTO   OF GFCTB092.
           MOVE COBR-SEQ-ARQ-ORIG      OF ATLZINDV
                                       TO CSEQ-MOVTO       OF GFCTB092.

           EXEC SQL
             SELECT
                   DENVIO_MOVTO_TARIF,
                   CROTNA_ORIGE_MOVTO,
                   CNRO_ARQ_MOVTO,
                   CSEQ_MOVTO,
                   CCTA_DSTNO_MOVTO,
                   CAG_CTLZA_DEB,
                   CCTA_CTLZA_DEB,
                   VTARIF_LIQ_MOVTO,
                   VTARIF_DEB_MOVTO,
                   DAGNDA_DEB_MOVTO,
                   DEFETV_DEB_MOVTO,
                   QMAX_TENTV_DEB,
                   QTENTV_DEB_MOVTO,
                   CSIT_EVNTO_RECBD,
                   DOCOR_EVNTO
             INTO
                   :GFCTB092.DENVIO-MOVTO-TARIF,
                   :GFCTB092.CROTNA-ORIGE-MOVTO,
                   :GFCTB092.CNRO-ARQ-MOVTO,
                   :GFCTB092.CSEQ-MOVTO,
                   :GFCTB092.CCTA-DSTNO-MOVTO,
                   :GFCTB092.CAG-CTLZA-DEB,
                   :GFCTB092.CCTA-CTLZA-DEB,
                   :GFCTB092.VTARIF-LIQ-MOVTO,
                   :GFCTB092.VTARIF-DEB-MOVTO,
                   :GFCTB092.DAGNDA-DEB-MOVTO,
                   :GFCTB092.DEFETV-DEB-MOVTO,
                   :GFCTB092.QMAX-TENTV-DEB,
                   :GFCTB092.QTENTV-DEB-MOVTO,
                   :GFCTB092.CSIT-EVNTO-RECBD,
                   :GFCTB092.DOCOR-EVNTO
             FROM   DB2PRD.MOVTO_EVNTO_CRRTT
             WHERE
                   DENVIO_MOVTO_TARIF = :GFCTB092.DENVIO-MOVTO-TARIF AND
                   CROTNA_ORIGE_MOVTO = :GFCTB092.CROTNA-ORIGE-MOVTO AND
                   CNRO_ARQ_MOVTO     = :GFCTB092.CNRO-ARQ-MOVTO     AND
                   CSEQ_MOVTO         = :GFCTB092.CSEQ-MOVTO
           END-EXEC.
GP36
..         IF (SQLCODE               NOT EQUAL ZEROS AND +100 AND -811)
..             MOVE  'DB2'               TO  ERR-TIPO-ACESSO
..             MOVE  'MOVTO_EVNTO_CRRTT' TO  ERR-DBD-TAB
..             MOVE  'SELECT'            TO  ERR-FUN-COMANDO
..             MOVE  SQLCODE             TO  ERR-SQL-CODE
..             MOVE  '0001'              TO  ERR-LOCAL
..             MOVE  SPACES              TO  ERR-SEGM
..             PERFORM 9999-PROCESSAR-ROTINA-ERRO
GP36       END-IF.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE REG-COBRANCA       OF ATLZINDV
                                       TO REG-COBRANCA     OF OCOR1188
               MOVE 'GFCT0516'         TO COBR-JOB         OF OCOR1188
               MOVE 'GFCT1188'         TO COBR-PROGRAMA    OF OCOR1188
               MOVE 'GFCTB092'         TO COBR-RECURSO     OF OCOR1188
               MOVE SQLCODE            TO WRK-SQLCODE
               MOVE WRK-SQLCODE        TO COBR-COD-RETORNO OF OCOR1188
               MOVE 'ERRO NO SELECT DA TABELA'
                                       TO COBR-DESCRICAO   OF OCOR1188
0321           MOVE 14                 TO COBR-SITUACAO-EVENTO
0321                                   OF OCOR1188
               WRITE REG-COBRANCA      OF OCOR1188
               PERFORM 1140-TESTAR-FS-OCOR1188
               ADD 1                   TO ACU-GRAV-OCOR1188
           END-IF.

0322       IF  COBR-COD-RETORNO        OF ATLZINDV
0322                                      EQUAL '001' OR '012'
0322           MOVE ZEROS              TO
0322                                   COBR-VALOR-DEBITADO OF ATLZINDV
0322       END-IF.

           IF  VTARIF-DEB-MOVTO OF GFCTB092 NOT NUMERIC
               MOVE  ZEROS             TO VTARIF-DEB-MOVTO
                                       OF GFCTB092
           END-IF.

0322       ADD COBR-VALOR-DEBITADO     OF ATLZINDV
0322                                   TO VTARIF-DEB-MOVTO
0322                                   OF GFCTB092.

070510     MOVE DEFETV-DEB-MOVTO       OF GFCTB092
070510                                 TO WRK-DEFETV-DEB-MOVTO.

WS         MOVE GFCT06-DPROCM-ANTER    TO DEFETV-DEB-MOVTO
WS                                     OF GFCTB092.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA DE ATUALIZACAO DOS REGISTROS DAS BASES GFCT             *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-EVENTO-COBRADO-TOTAL       SECTION.
      *----------------------------------------------------------------*

           MOVE 5                      TO WRK-CSIT-EVENTO-5525.

           PERFORM 3210-ATUALIZAR-GFCTB092.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 3220-ATUALIZAR-SIT-EVENTO
               PERFORM 3230-GRAVAR-FITA4
2303           IF   COBR-VALOR-DEBITADO     OF ATLZINDV
2303                                 NOT EQUAL VTARIF-LIQ-MOVTO
2303                                        OF GFCTB092
2303                PERFORM 3321-GRAVAR-COBR-PARCIAL
2303           END-IF
           END-IF.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA DE ATUALIZACAO DA TABELA GFCTB092                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3210-ATUALIZAR-GFCTB092         SECTION.
      *----------------------------------------------------------------*

070510     IF   WRK-RETORNO-F     EQUAL ZEROS
070510          MOVE WRK-CSIT-EVENTO-5525
071101                              TO CSIT-EVNTO-RECBD OF GFCTB092
070510     ELSE
070510          MOVE WRK-RETORNO-F
071101                              TO CSIT-EVNTO-RECBD OF GFCTB092
070510     END-IF.

WS    *---> ATUALIZA AGENCIA/CONTA NO ARQUIVO ATLZINDIV
           MOVE COBR-AGENCIA-RET    OF ATLZINDV
                                    TO COBR-AGENCIA-DEB OF ATLZINDV.
           MOVE COBR-CONTA-RET      OF ATLZINDV
                                    TO COBR-CONTA-DEB   OF ATLZINDV.

WS    *---> ATUALIZA AGENCIA/CONTA NA TABELA GFCTB092
           IF COBR-CENTRALIZ        OF ATLZINDV  EQUAL 'S'
              MOVE COBR-AGENCIA-RET OF ATLZINDV  TO
071101                                           CAG-CTLZA-DEB
              MOVE COBR-CONTA-RET   OF ATLZINDV  TO
071101                                           CCTA-CTLZA-DEB
           ELSE
              MOVE COBR-AGENCIA-RET OF ATLZINDV  TO
071101                                           CAG-DSTNO-MOVTO
              MOVE COBR-CONTA-RET   OF ATLZINDV  TO
071101                                           CCTA-DSTNO-MOVTO
           END-IF.

071101     MOVE DENVIO-MOVTO-TARIF TO GFCT67-DENVIO-MOVTO-TARIF
071101     MOVE CROTNA-ORIGE-MOVTO TO GFCT67-CROTNA-ORIGE-MOVTO
071101     MOVE CNRO-ARQ-MOVTO     TO GFCT67-CNRO-ARQ-MOVTO
071101     MOVE CSEQ-MOVTO         TO GFCT67-CSEQ-MOVTO
071101     MOVE CAG-DSTNO-MOVTO    TO GFCT67-CAG-DSTNO-MOVTO
071101     MOVE CCTA-DSTNO-MOVTO   TO GFCT67-CCTA-DSTNO-MOVTO
071101     MOVE CAG-CTLZA-DEB      TO GFCT67-CAG-CTLZA-DEB
071101     MOVE CCTA-CTLZA-DEB     TO GFCT67-CCTA-CTLZA-DEB
071101     MOVE QTENTV-DEB-MOVTO   TO GFCT67-QTENTV-DEB-MOVTO
071101     MOVE DAGNDA-DEB-MOVTO   TO GFCT67-DAGNDA-DEB-MOVTO
071101     MOVE VTARIF-DEB-MOVTO   TO GFCT67-VTARIF-DEB-MOVTO
071101     MOVE DEFETV-DEB-MOVTO   TO GFCT67-DEFETV-DEB-MOVTO
071101     MOVE CSIT-EVNTO-RECBD   TO GFCT67-CSIT-EVNTO-RECBD.

BI1210     MOVE COBR-NUM-PARC      OF ATLZINDV
BI1210                             TO GFCT67-NUM-PARC.

071101     WRITE GFCT67-GFCTB092.
071101
071101     PERFORM 1180-TESTAR-FS-ARQATU92.
071101
071101     ADD 1                       TO ACU-GRAV-ARQATU92.

      *---> GRAVACAO DO ARQUIVO ARQCOBR2.

070920     MOVE REG-COBRANCA     OF ATLZINDV TO REG-GFCTMW.
070920     MOVE QMAX-TENTV-DEB   OF GFCTB092 TO GFCTMW-TENTV-DEB-MAX.
070920     MOVE VTARIF-LIQ-MOVTO OF GFCTB092 TO GFCTMW-VALOR-LIQ92.
070920     MOVE QTENTV-DEB-MOVTO OF GFCTB092 TO GFCTMW-TENTV-DEB-ATU.
070920     MOVE VTARIF-DEB-MOVTO OF GFCTB092 TO GFCTMW-VALOR-COBR92.
070920     MOVE CSIT-EVNTO-RECBD OF GFCTB092 TO GFCTMW-SITUACAO-EVENTO.
070920
070920     WRITE REG-GFCTMW.
070920
070920     PERFORM 1170-TESTAR-FS-ARQCOBR2.
070920
070920     ADD 1                       TO ACU-GRAV-ARQCOBR2.

      *----------------------------------------------------------------*
       3210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3220-ATUALIZAR-SIT-EVENTO       SECTION.
      *----------------------------------------------------------------*

2703       MOVE DENVIO-MOVTO-TARIF     OF GFCTB092
2703                                   TO ZD-DENVIO-MOVTO-TARIF.
2703       MOVE CROTNA-ORIGE-MOVTO     OF GFCTB092
2703                                   TO ZD-CROTNA-ORIGE-MOVTO.
2703       MOVE CNRO-ARQ-MOVTO         OF GFCTB092
2703                                   TO ZD-CNRO-ARQ-MOVTO.
2703       MOVE CSEQ-MOVTO             OF GFCTB092
2703                                   TO ZD-CSEQ-MOVTO.
2703       MOVE WRK-TIMESTAMP-H4       TO ZD-HTROCA-SIT-EVNTO.
2703       MOVE WRK-CSIT-EVENTO-5525   TO ZD-CSIT-EVNTO-RECBD.
2703

           MOVE WRK-TIMESTAMP-H4       TO WRK-CPTCAO-GFCTB0H4
           MOVE WRK-CPTCAO-B0H4        TO ZD-CPTCAO-TBELA

2703       WRITE ZD-REGISTRO.
2703
2703       PERFORM 1150-TESTAR-FS-ATLZB0H4.
2703
2703       ADD 1                       TO ACU-GRAV-ATLZB0H4.

      *----------------------------------------------------------------*
       3220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA GRAVAR ARQUIVO PARA GERACAO DA FITA4               *
      ******************************************************************
      *----------------------------------------------------------------*
       3230-GRAVAR-FITA4               SECTION.
      *----------------------------------------------------------------*

           MOVE COBR-DATA-ORIG         OF ATLZINDV
                                       TO GFCTJH-DT-ENVIO-MOVTO.
           MOVE COBR-ROTINA-ORIG       OF ATLZINDV
                                       TO GFCTJH-CD-ROTINA-ORIGEM
           MOVE COBR-NUM-ARQ-ORIG      OF ATLZINDV
                                       TO GFCTJH-CD-NUMERO-ARQUIVO.
           MOVE COBR-SEQ-ARQ-ORIG      OF ATLZINDV
                                       TO GFCTJH-CD-SEQ-MOVTO.
           MOVE 4                      TO GFCTJH-CD-ORIGEM
                                          GFCTJH-CD-TIPO-SOLTC.
           MOVE COBR-TARIFA-ORIG       OF ATLZINDV
                                       TO GFCTJH-TARIFA.

           ADD  COBR-VALOR-DEBITADO    OF ATLZINDV
                                       TO ACU-VALOR-CONTABIL.

           MOVE COBR-VALOR-DEBITADO    OF ATLZINDV
                                       TO GFCTJH-VALOR.
           MOVE COBR-BANCO-DEB         OF ATLZINDV
                                       TO GFCTJH-BANCO.
           MOVE COBR-AGENCIA-DEB       OF ATLZINDV
                                       TO GFCTJH-AGENCIA.
           MOVE COBR-CONTA-DEB         OF ATLZINDV
                                       TO GFCTJH-CONTA.
           MOVE COBR-BANCO-ORIG        OF ATLZINDV
                                       TO GFCTJH-BANCO-ORIGEM.
           MOVE COBR-AGENCIA-ORIG      OF ATLZINDV
                                       TO GFCTJH-AGENCIA-ORIGEM.
           MOVE COBR-DATA-EVNTO-ORIG   OF ATLZINDV
                                       TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-DB2.
           MOVE WRK-MES-AUX            TO WRK-MES-DB2.
           MOVE WRK-ANO-AUX            TO WRK-ANO-DB2.
           MOVE WRK-DATA-DB2           TO GFCTJH-DATA-EVENTO.

           WRITE GFCTJH-ENTRADA.

           PERFORM 1130-TESTAR-FS-FITCONTB.

           ADD 1                       TO ACU-GRAV-FITCONTB.

           PERFORM 3232-GRAVAR-ARQDEBIT.

      *----------------------------------------------------------------*
       3230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

070511******************************************************************
070511* ROTINA PARA GRAVAR ARQUIVO DE EVENTOS EFETIVAMENTE COBRADOS    *
070511******************************************************************
070511*----------------------------------------------------------------*
070511 3232-GRAVAR-ARQDEBIT            SECTION.
070511*----------------------------------------------------------------*
070511
070511     MOVE COBR-AGENCIA-DEB       OF ATLZINDV
070511                                 TO GFCTZJ-COD-AGENCIA.
070511     MOVE COBR-CONTA-DEB         OF ATLZINDV
070511                                 TO GFCTZJ-NUM-CONTA.
070511     MOVE COBR-GRUPO-DEB         OF ATLZINDV
070511                                 TO GFCTZJ-COD-GRUPO.
070511     MOVE COBR-SUBGRUPO-DEB      OF ATLZINDV
070511                                 TO GFCTZJ-COD-SUB-GRUPO.
070511     MOVE COBR-CGC-CPF-MUNERO    OF ATLZINDV
070511                                 TO GFCTZJ-CNPJ-CPF-PRINCIPAL.
070511     MOVE COBR-CGC-CPF-FILIAL    OF ATLZINDV
070511                                 TO GFCTZJ-CNPJ-CPF-FILIAL.
070511     MOVE COBR-CGC-CPF-CONTROLE  OF ATLZINDV
070511                                 TO GFCTZJ-CNPJ-CPF-CONTROLE.
070511     MOVE COBR-TARIFA-ORIG       OF ATLZINDV
070511                                 TO GFCTZJ-COD-EVENTO-TARIFA.
070511     MOVE DEFETV-DEB-MOVTO       OF GFCTB092
070511                                 TO WRK-DATA-DEB.
070511     MOVE WRK-DIA-DEB            TO GFCTZJ-DIA-DEBITO.
070511     MOVE WRK-MES-DEB            TO GFCTZJ-MES-DEBITO.
070511     MOVE WRK-ANO-DEB            TO GFCTZJ-ANO-DEBITO.
070604     MOVE DOCOR-EVNTO            OF GFCTB092
070604                                 TO WRK-DATA-DEB.
070604     MOVE WRK-DIA-DEB            TO GFCTZJ-DIA-EVENTO.
070604     MOVE WRK-MES-DEB            TO GFCTZJ-MES-EVENTO.
070604     MOVE WRK-ANO-DEB            TO GFCTZJ-ANO-EVENTO.
070511     MOVE ZEROS                  TO GFCTZJ-COD-IDENT-COBRANCA.
070511     MOVE COBR-VALOR-DEBITADO    OF ATLZINDV
070511                                 TO GFCTZJ-VLR-COBRADO.
070511
070511     IF   COBR-COD-RETORNO       OF ATLZINDV
070511                                 IS NUMERIC
070511          MOVE 1                 TO GFCTZJ-COD-IDENT-ORIG-DEB
070511     ELSE
070511          MOVE 2                 TO GFCTZJ-COD-IDENT-ORIG-DEB
070511     END-IF.
070511
070511     WRITE GFCTZJ-REGISTRO.
070511
070511     PERFORM 1160-TESTAR-FS-ARQDEBIT.
070511
070511     ADD 1                       TO ACU-GRAV-ARQDEBIT.
070511
070511*----------------------------------------------------------------*
070511 3232-99-FIM.                    EXIT.
070511*----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA ATUALIZAR A SITUACAO DO REGISTRO DAS BASES GFCT    *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-EVENTO-COBRADO-PARCIAL     SECTION.
      *----------------------------------------------------------------*

           IF  QTENTV-DEB-MOVTO        OF GFCTB092
0321                             NOT LESS QMAX-TENTV-DEB
                                       OF GFCTB092
               PERFORM 3310-FIM-REPIQUE
           ELSE
               PERFORM 3320-REPIQUE
           END-IF.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA ATUALIZAR A SITUACAO DO REGISTRO DAS BASES DB2     *
      ******************************************************************
      *----------------------------------------------------------------*
       3310-FIM-REPIQUE                SECTION.
      *----------------------------------------------------------------*

2103       IF  VTARIF-DEB-MOVTO        OF GFCTB092
2103                              GREATER ZEROS
2103           MOVE 4                  TO WRK-CSIT-EVENTO-5525
2103       ELSE
2103           MOVE 12                 TO WRK-CSIT-EVENTO-5525
2103       END-IF.

           PERFORM 3210-ATUALIZAR-GFCTB092.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 3220-ATUALIZAR-SIT-EVENTO
WS             IF  COBR-VALOR-DEBITADO  OF ATLZINDV
WS                                    GREATER ZEROS
WS                 PERFORM 3230-GRAVAR-FITA4
WS                 PERFORM 3321-GRAVAR-COBR-PARCIAL
WS             END-IF
           END-IF.

      *----------------------------------------------------------------*
       3310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA PREPARAR REGISTRO PARA PROXIMA COBRANCA            *
      ******************************************************************
      *----------------------------------------------------------------*
       3320-REPIQUE                    SECTION.
      *----------------------------------------------------------------*

           MOVE 3                          TO WRK-CSIT-EVENTO-5525.

           ADD 1                           TO QTENTV-DEB-MOVTO
                                           OF GFCTB092.

           PERFORM 3210-ATUALIZAR-GFCTB092.

           IF  SQLCODE                     EQUAL ZEROS
WS             IF (COBR-VALOR-DEBITADO     OF ATLZINDV
                                           GREATER ZEROS AND
                   RETURN-CODE             EQUAL ZEROS   AND
                   WRK-5525-COD-RETORNO    EQUAL ZEROS)
070612             PERFORM 3220-ATUALIZAR-SIT-EVENTO
                   PERFORM 3230-GRAVAR-FITA4
                   PERFORM 3321-GRAVAR-COBR-PARCIAL
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA INSERIR  REGISTRO DE  COBRANCA PARCIAL  NA TABELA  *
      * GFCTB0H7                                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3321-GRAVAR-COBR-PARCIAL        SECTION.
      *----------------------------------------------------------------*

           MOVE COBR-DATA-ORIG         OF ATLZINDV
                                       TO GFCT68-DENVIO-MOVTO-TARIF
           MOVE COBR-ROTINA-ORIG       OF ATLZINDV
                                       TO GFCT68-CROTNA-ORIGE-MOVTO
           MOVE COBR-NUM-ARQ-ORIG      OF ATLZINDV
                                       TO GFCT68-CNRO-ARQ-MOVTO
           MOVE COBR-SEQ-ARQ-ORIG      OF ATLZINDV
                                       TO GFCT68-CSEQ-MOVTO
           MOVE COBR-VALOR-DEBITADO    OF ATLZINDV
                                       TO GFCT68-VDEB-PCIAL-TARIF
           MOVE GFCT06-DPROCM-ANTER    TO GFCT68-DDEB-PCIAL-TARIF

           WRITE GFCT68-GFCTB0H7.

           PERFORM 1190-TESTAR-FS-ARQATUH7.

           ADD 1                       TO ACU-GRAV-ARQATUH7.

      *----------------------------------------------------------------*
       3321-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA NAO ATUALIZAR SIT EVENTO NO FERIADO AGENCIA POUP   *
      ******************************************************************
      *----------------------------------------------------------------*
       3400-RETORNAR-SIT-EVENTO        SECTION.
      *----------------------------------------------------------------*
070510     MOVE WRK-DEFETV-DEB-MOVTO      TO  DEFETV-DEB-MOVTO
070510                                    OF  GFCTB092.
070510
           IF  COBR-COD-RETORNO   OF ATLZINDV EQUAL '009'
               MOVE 3                    TO  WRK-RETORNO-F
           ELSE
              IF   WRK-DEFETV-DEB-MOVTO       = '01.01.0001'
                   MOVE 2                    TO  WRK-RETORNO-F
              ELSE
                   MOVE 3                    TO  WRK-RETORNO-F
              END-IF
           END-IF.
070510
070510     PERFORM 3210-ATUALIZAR-GFCTB092.
070510     ADD     1                      TO  ACU-FERIADO-AGEN.
070510     MOVE    ZEROS                  TO  WRK-RETORNO-F.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA FINALIZACAO DO PROGRAMA                            *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-DISPLAY.

           CLOSE  ATLZINDV
                  CTRLDATA
                  FITCONTB
                  OCOR1188
2703              ATLZB0H4
070511            ARQDEBIT
071101            ARQCOBR2
071101            ARQATU92
                  ARQATUH7.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS-ATLZINDV     TO WRK-MASCARA.

           DISPLAY '******************** GFCT1188 ********************'.
           DISPLAY '*                                                *'.
           DISPLAY '* REGISTROS LIDOS EM ATLZINDV...: ' WRK-MASCARA
                                                                   ' *'.

           MOVE ACU-LIDOS-CTRLDATA     TO WRK-MASCARA.

           DISPLAY '* REGISTROS LIDOS EM CTRLDATA...: ' WRK-MASCARA
                                                                   ' *'.
           MOVE ACU-GRAV-FITCONTB      TO WRK-MASCARA.

           DISPLAY '* REGISTROS GRAVADOS EM FITCONTB: ' WRK-MASCARA
                                                                   ' *'.

           MOVE ACU-GRAV-OCOR1188      TO WRK-MASCARA.

           DISPLAY '* REGISTROS GRAVADOS EM OCOR1188: ' WRK-MASCARA
                                                                   ' *'.

071101     MOVE ACU-GRAV-ARQATU92      TO WRK-MASCARA.

           DISPLAY '* REGISTROS A ATUALIZAR GFCTB092: ' WRK-MASCARA
                                                                   ' *'.
071101     MOVE ACU-GRAV-ARQATUH7      TO WRK-MASCARA.

071101     DISPLAY '* REGISTROS A ATUALIZAR GFCTB0H7: ' WRK-MASCARA
                                                                   ' *'.

070510     MOVE ACU-FERIADO-AGEN       TO WRK-MASCARA.

070510     DISPLAY '* FERIADO NA AGENCIA POUP/MORA  : ' WRK-MASCARA
                                                                   ' *'.

2703       MOVE ACU-GRAV-ATLZB0H4      TO WRK-MASCARA.
2703
2703       DISPLAY '* REGISTROS A ATUALIZAR GFCTB0H4: ' WRK-MASCARA
2603                                                               ' *'.

070511     MOVE ACU-GRAV-ARQDEBIT      TO WRK-MASCARA.
070511
070511     DISPLAY '* REGISTROS GRAVADOS EM ARQDEBIT: ' WRK-MASCARA
060511                                                             ' *'.

070920     MOVE ACU-GRAV-ARQCOBR2      TO WRK-MASCARA.
070920
070920     DISPLAY '* REGISTROS GRAVADOS EM ARQCOBR2: ' WRK-MASCARA
060920                                                             ' *'.

           MOVE ACU-DESPREZADOS        TO WRK-MASCARA.

           DISPLAY '* REGISTROS DESPREZADOS.........: ' WRK-MASCARA
                                                                   ' *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT1188 ********************'.

           MOVE ACU-VALOR-CONTABIL        TO WRK-MASCAR1.
           DISPLAY '                                                  '.
           DISPLAY '******************** GFCT1188 ********************'.
           DISPLAY '*                                                *'.
           DISPLAY '* TOTAL CONTABILIZADO...........: ' WRK-MASCAR1
                                                                   ' *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT1188 ********************'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA PROCESSAR ERROS                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 4                      TO RETURN-CODE.

           MOVE 'GFCT1188'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
