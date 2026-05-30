      *===============================================================*
       IDENTIFICATION DIVISION.
      *===============================================================*

       PROGRAM-ID. RUECC890.
       AUTHOR.     EDUARDO WATANABE.

      ******************************************************************
      *    ANALISTA   :        EDUARDO WATANABE                        *
      *    DATA       :        FEVEREIRO/2016                          *
      *----------------------------------------------------------------*
      *    PROJETO    :        BSI 99/9999 - CATALOGO 4.07             *
      *----------------------------------------------------------------*
      *    OBJETIVO :                                                  *
      *                                                                *
      *       ENVIAR COR0001A DAS LIBERACOES.                          *
      *       PROGRAMA CLONE NO RUEC0890                               *
      *----------------------------------------------------------------*
      *    ENTRADA :                                                   *
      *                                                                *
      *      ARQUIVOS:                                                 *
      *         DDNAME    DESCRICAO                       INCLUDE/BOOK *
      *         ARQLIBER  ARQUIVO LIBERADOS NO DIA        --------     *
      *         ARQNCOR1  ARQUIVO MENSAGEM COR0001 NAS    RUECWCOR     *
      *                                                                *
      *----------------------------------------------------------------*
      *    SAIDA :                                                     *
      *                                                                *
      *      ARQUIVOS:                                                 *
      *         DDNAME    DESCRICAO                       INCLUDE/BOOK *
      *         ARQCOR1A  ARQUIVO MENSAGEM COR0001A       --------     *
      *                                                                *
      *----------------------------------------------------------------*
      *    DB2 :                                                       *
      *                                                                *
      *      TABELA:                                                   *
      *         DDNAME    DESCRICAO                  NOME DA TABELA    *
      *       RUFIB002    CONTRATO                   CONTRATO_RURAL    *
      *       RUECB09A    ENVIO RECOR                TENVIO_RECOR_TEMPR*
      *                                                                *
      *----------------------------------------------------------------*
      *    MODULOS CHAMADOS :                                          *
      *                                                                *
      *       POOL7100 - TRATAMENTO DE ERROS DE APLICACAO.             *
      *       POOL7600 - OBTER DATA DO SISTEMA.                        *
      *                                                                *
      ******************************************************************
CAT408*================================================================*
CAT408*                      A L T E R A C A O                         *
CAT408*================================================================*
CAT408*    ANALISTA....:   EVANDRO GUIMARAES       - BSI TECNOLOGIA    *
CAT408*    DATA........:   SETEMBRO/2016                               *
CAT408*    OBJETIVO....:   ADAPTACAO CATALOGO 4.08                     *
CAT408*================================================================*
CAT408
BR0618*================================================================*
BR0618*                      A L T E R A C A O                         *
BR0618*================================================================*
BR0618*    ANALISTA....:   PATRICK SILVEIRA        - BRQ               *
BR0618*    DATA........:   JUNHO/2018                                  *
BR0618*    OBJETIVO....:   ADPATAR BOOK I#RUEC66                       *
BR0618*================================================================*
BR0618
QT1120*================================================================*
QT1120*                    ALTERACAO - QINTESS                         *
QT1120*----------------------------------------------------------------*
QT1120*  ANALISTA QINTESS: KELLY CHUCATA                               *
QT1120*  DATA...........:  13/11/2020                                  *
QT1120*----------------------------------------------------------------*
QT1120*  OBJETIVO......:   ADEQUAR PARA SICOR 5.01                     *
QT1120*    ALTERACOES NA COR0005:                                      *
QT1120*                    - RETIRADA DO CAMPO "PERCENTUAL DESCLASSIFI *
QT1120*                                         CACAO"                 *
QT1120*================================================================*
           EJECT
      *===============================================================*
       ENVIRONMENT DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       CONFIGURATION SECTION.
      *---------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *---------------------------------------------------------------*
       INPUT-OUTPUT SECTION.
      *---------------------------------------------------------------*

       FILE-CONTROL.

           SELECT ARQLIBER ASSIGN TO UT-S-ARQLIBER
                      FILE STATUS IS WRK-FS-ARQLIBER.

           SELECT ARQNCOR1 ASSIGN TO UT-S-ARQNCOR1
                      FILE STATUS IS WRK-FS-ARQNCOR1.

           SELECT ARQCOR1A ASSIGN TO UT-S-ARQCOR1A
                      FILE STATUS IS WRK-FS-ARQCOR1A.

      *===============================================================*
       DATA DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *    INPUT:     ARQUIVO LIBERADOS NO DIA                        *
      *               ORG. SEQUENCIAL   -   LRECL = 750               *
      *---------------------------------------------------------------*

       FD  ARQLIBER
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

CAT408 01  FD-ARQLIBER                 PIC  X(1259).

      *---------------------------------------------------------------*
      *    INPUT:     ARQUIVO MENSAGEM COR0001 NAS                    *
      *               ORG. SEQUENCIAL   -   LRECL = 32593             *
      *---------------------------------------------------------------*

       FD  ARQNCOR1
           RECORDING MODE IS V
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ARQNCOR1.
       COPY RUECWCOR.

      *---------------------------------------------------------------*
      *    OUTPUT:    ARQUIVO MENSAGEM COR0001A                       *
      *               ORG. SEQUENCIAL   -   LRECL = 32604             *
      *---------------------------------------------------------------*

       FD  ARQCOR1A
           RECORDING MODE IS V
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ARQCOR1A.
           05                          PIC  X(09).
           05  FD-ARQCOR1A-TAM         PIC S9(04) COMP.
           05  FD-ARQCOR1A-TXT         PIC  X(01) OCCURS 1 TO 32589
                                       DEPENDING ON FD-ARQCOR1A-TAM.

      *---------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** RUECC890 - INICIO DA AREA DE WORKING ***'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** ACUMULADORES ***'.
      *---------------------------------------------------------------*

       01  WRK-AREA-RESTART.
           05  ACU-LIDOS-ARQLIBER      PIC  9(009) COMP-3  VALUE ZEROS.
           05  ACU-LIDOS-ARQNCOR1      PIC  9(009) COMP-3  VALUE ZEROS.
           05  ACU-DESPR-VERS-ANT      PIC  9(009) COMP-3  VALUE ZEROS.
           05  ACU-GRAVS-ARQCOR1A      PIC  9(009) COMP-3  VALUE ZEROS.
           05  ACU-RCOR-OK             PIC  9(009) COMP-3  VALUE ZEROS.
           05  ACU-RCOR-NOK            PIC  9(009) COMP-3  VALUE ZEROS.

       77  WRK-LIDOS-ARQLIBER-E        PIC  ZZZ.ZZZ.ZZ9    VALUE SPACES.
       77  WRK-LIDOS-ARQNCOR1-E        PIC  ZZZ.ZZZ.ZZ9    VALUE SPACES.
       77  WRK-DESPR-VERS-ANT-E        PIC  ZZZ.ZZZ.ZZ9    VALUE SPACES.
       77  WRK-GRAVS-ARQCOR1A-E        PIC  ZZZ.ZZZ.ZZ9    VALUE SPACES.
       77  WRK-RCOR-OK-E               PIC  ZZZ.ZZZ.ZZ9    VALUE SPACES.
       77  WRK-RCOR-NOK-E              PIC  ZZZ.ZZZ.ZZ9    VALUE SPACES.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA TESTES DE FILE STATUS ***'.
      *---------------------------------------------------------------*

       77  WRK-FS-ARQLIBER             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ARQNCOR1             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ARQCOR1A             PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(010)         VALUE
           ' ABERTURA '.
       77  WRK-LEITURA                 PIC  X(010)         VALUE
           ' LEITURA  '.
       77  WRK-GRAVACAO                PIC  X(010)         VALUE
           ' GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(010)         VALUE
           'FECHAMENTO'.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES ***'.
      *---------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

       77  WRK-MODULO-RCOR5005     PIC  X(08) VALUE 'RCOR5005'.

       01  WRK-CHAVE-ARQLIBER.
           05  WRK-CHV-LIB-CCONTR      PIC  9(09)          VALUE ZEROS.

       01  WRK-CHAVE-ARQNCOR1.
           05  WRK-CHV-NC1-CCONTR      PIC  9(09)          VALUE ZEROS.

       01  WRK-DT-VERSAO-CATALOGO      PIC  X(10)          VALUE SPACES.
       01  WRK-IND                     PIC  9(03) COMP-3   VALUE ZEROS.
       01  WRK-POSICAO                 PIC  9(05) COMP-3   VALUE ZEROS.
       01  WRK-FIM-LIB                 PIC  9(05) COMP-3   VALUE ZEROS.
       01  WRK-MENSAGEM                PIC  X(32500)       VALUE SPACES.
CAT408
CAT408*---------------------------------------------------------------*
CAT408 77  FILLER                      PIC  X(032)         VALUE
CAT408     '*** AUXILIARES CAT4.08       ***'.
CAT408*---------------------------------------------------------------*
CAT408
CAT408 01  WRK-NULL-RUECB099.
CAT408     03  WRK-NULL-DCDULA             PIC S9(04) COMP VALUE ZEROS.
CAT408     03  WRK-NULL-DVCTO-CONTR        PIC S9(04) COMP VALUE ZEROS.
CAT408     03  WRK-NULL-VFINAN             PIC S9(04) COMP VALUE ZEROS.
CAT408     03  WRK-NULL-CCATEG-PROTR-RECOR PIC S9(04) COMP VALUE ZEROS.
CAT408     03  WRK-NULL-CPROG-RURAL        PIC S9(04) COMP VALUE ZEROS.
CAT408     03  WRK-NULL-CMUN-REFT-BACEN    PIC S9(04) COMP VALUE ZEROS.
CAT408     03  WRK-NULL-VPCELA-LIBRC       PIC S9(04) COMP VALUE ZEROS.
CAT408     03  WRK-NULL-VPCELA-REC-PPRIO   PIC S9(04) COMP VALUE ZEROS.
CAT408     03  WRK-NULL-QUND-FINCD-RURAL   PIC S9(04) COMP VALUE ZEROS.
CAT408     03  WRK-NULL-QUND-PROD-PROVV    PIC S9(04) COMP VALUE ZEROS.
CAT408     03  WRK-NULL-DINIC-SAFRA        PIC S9(04) COMP VALUE ZEROS.
CAT408     03  WRK-NULL-DFIM-SAFRA         PIC S9(04) COMP VALUE ZEROS.
CAT408     03  WRK-NULL-CTPO-GARNT-RECOR   PIC S9(04) COMP VALUE ZEROS.
CAT408
CAT408 01  WRK-NULL-RUECB09A.
CAT408     03  WRK-NULL-CSIT-MSGEM         PIC S9(04) COMP VALUE ZEROS.
CAT408     03  WRK-NULL-HINCL              PIC S9(04) COMP VALUE ZEROS.
CAT408
CAT408*---------------------------------------------------------------*
CAT408 01  FILLER                      PIC X(032)          VALUE
CAT408     '*** WRK-MENSAGEM-408 32500B  ***'.
CAT408*---------------------------------------------------------------*
CAT408
CAT408 01  WRK-MENSAGEM-408            PIC  X(32500)       VALUE SPACES.
CAT408
CAT408*---------------------------------------------------------------*
CAT408 01  FILLER                      PIC X(032)          VALUE
CAT408     '*** WRK-BRANCOS - 32500 BYTES***'.
CAT408*---------------------------------------------------------------*
CAT408
CAT408 01  WRK-BRANCOS                 PIC  X(32500)       VALUE SPACES.
CAT408
CAT408*---------------------------------------------------------------*
CAT408 01  FILLER                      PIC X(032)          VALUE
CAT408     '*** CONVERSAO - DEMAIS AREAS ***'.
CAT408*---------------------------------------------------------------*
CAT408
CAT408 01  WRK-VERSAO-407              PIC  X(01)          VALUE SPACES.
CAT408 01  WRK-POSICAO-408             PIC  9(05) COMP-3   VALUE ZEROS.
CAT408 01  WRK-TAMANHO-DESC            PIC  9(05) COMP-3   VALUE ZEROS.
CAT408
CAT408 01  WRK-47-COR0001-7.
CAT408     05 WRK-47-GRUPO-COR0001-PTPOLG.
CAT408        10 FILLER                PIC  X(006)      VALUE '{2210}'.
CAT408        10 WRK-47-GP-COR0001-PTPOLG.
CAT408           15 WRK-47-LAT-PONTO   PIC  X(008)        VALUE SPACES.
CAT408           15 WRK-47-LAT-PONTO-N REDEFINES
CAT408              WRK-47-LAT-PONTO   PIC S9(02)V9(06).
CAT408           15 WRK-47-LONG-PONTO  PIC  X(009)        VALUE SPACES.
CAT408           15 WRK-47-LONG-PONTO-N REDEFINES
CAT408              WRK-47-LONG-PONTO  PIC S9(03)V9(06).
CAT408           15 WRK-47-ALTT-PONTO  PIC  X(006)        VALUE SPACES.
CAT408
CAT408 01  WRK-48-COR0001-7.
CAT408     05 WRK-48-GRUPO-COR0001-PTPOLG.
CAT408        10 FILLER                PIC  X(006)      VALUE '{2210}'.
CAT408        10 WRK-48-GP-COR0001-PTPOLG.
CAT408           15 WRK-48-LAT-PONTO   PIC  X(013)        VALUE SPACES.
CAT408           15 WRK-48-LAT-PONTO-N REDEFINES
CAT408              WRK-48-LAT-PONTO   PIC S9(02)V9(11).
CAT408           15 WRK-48-LONG-PONTO  PIC  X(014)        VALUE SPACES.
CAT408           15 WRK-48-LONG-PONTO-N REDEFINES
CAT408              WRK-48-LONG-PONTO  PIC S9(03)V9(11).
CAT408           15 WRK-48-ALTT-PONTO  PIC  X(006)        VALUE SPACES.
CAT408
CAT408 01  WRK-47-COR0001-2.
CAT408     05 WRK-47-GRUPO-COR0001-EMIT.
CAT408        10 FILLER                    PIC  X(006)   VALUE '{1000}'.
CAT408        10 WRK-47-GP-COR0001-EMIT.
CAT408           15 WRK-47-TP-PESSOA-EMIT  PIC  X(001)    VALUE SPACES.
CAT408           15 WRK-47-CNPJ-CPF-EMIT   PIC  9(014)    VALUE ZEROS.
CAT408           15 WRK-47-DAP-EMIT        PIC  X(025)    VALUE SPACES.
CAT408
CAT408 01  WRK-48-COR0001-2.
CAT408     05 WRK-48-GRUPO-COR0001-EMIT.
CAT408        10 FILLER                    PIC  X(006)   VALUE '{1000}'.
CAT408        10 WRK-48-GP-COR0001-EMIT.
CAT408           15 WRK-48-TP-PESSOA-EMIT  PIC  X(001)    VALUE SPACES.
CAT408           15 WRK-48-CNPJ-CPF-EMIT   PIC  9(014)    VALUE ZEROS.
CAT408           15 WRK-48-DAP-EMIT        PIC  X(025)    VALUE SPACES.
CAT408           15 WRK-48-TPO-BENF        PIC  X(003)    VALUE SPACES.
CAT408
      *---------------------------------------------------------------*
      *    AREA PARA FORMATACAO/REFORMATACAO PARA ENVIO DADOS RECOR   *
      *---------------------------------------------------------------*

       COPY I#RUEC66.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** AREA PARA CHAMADA MODULO RCOR5005 ***'.
      *---------------------------------------------------------------*

       COPY 'I#RCOR10'.

      *----------------------------------------------------------------*
      *                      AREA DO MODULO CKRS0100                   *
      *----------------------------------------------------------------*

       77  WRK-CKRS0100                PIC X(08) VALUE 'CKRS0100'.

       COPY 'I#CKRS01'.

      *---------------------------------------------------------------*
      *    VARIAVEIS PARA TRATAR NULIDADE DOS CAMPOS DAS TABELAS      *
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA COM LAYOUT DO ARQUIVO ARQLIBER ***'.
      *---------------------------------------------------------------*

CAT408 01  WRK-REG-ARQLIBER.
           03  WRK-LIB-CCONTR-X.
               07 WRK-LIB-CCONTR       PIC  9(009)         VALUE ZEROS.
CAT408     03  WRK-ARQLIBER.
CAT408         05 WRK-ARQ-CCONTR-X.
CAT408            07 WRK-ARQ-CCONTR    PIC  9(009)         VALUE ZEROS.
               05 WRK-LIB-IND          PIC  9(003) COMP-3  VALUE ZEROS.
               05 WRK-LIB-OCORRENCIAS  OCCURS  30 TIMES.
                  10  WRK-LIB-DATA     PIC  X(008)         VALUE ZEROS.
                  10  WRK-LIB-VALOR    PIC  X(015)         VALUE ZEROS.
CAT408         05 FILLER               PIC  X(049)         VALUE SPACES.
CAT408     03  WRK-ARQDESCL.
CAT408         05 WRK-DES-CCONTR-X.
CAT408            07 WRK-DES-CCONTR    PIC  9(009)         VALUE ZEROS.
CAT408         05 WRK-DES-VAZIO        PIC  X(001)         VALUE SPACES.
CAT408         05 WRK-DES-GRUPO        PIC  X(330)         VALUE SPACES.
CAT408         05 FILLER               PIC  X(160)         VALUE SPACES.
CAT408
      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA COM LAYOUT DO ARQUIVO ARQCOR1A ***'.
      *---------------------------------------------------------------*

       01  WRK-ARQCOR1A.
           05  WRK-SIS-CCONTR          PIC  9(09)        VALUE ZEROS.
           05  WRK-SIS-TAM             PIC S9(04) COMP   VALUE ZEROS.
           05  WRK-SIS-TXT             PIC  X(32500)     VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** MENSAGEM DE ERRO DE FILE STATUS ***'.
      *---------------------------------------------------------------*

       01  WRK-ERRO-ARQUIVO.
           03  FILLER                  PIC  X(011)         VALUE
               '** ARQUIVO='.
           03  WRK-NOME-ARQUIVO        PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(008)         VALUE
               ' FUNCAO='.
           03  WRK-OPERACAO            PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(013)         VALUE
               ' FILE-STATUS='.
           03  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE ' **'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *---------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(36) VALUE
           '* AREA MENSAGEM DE DATA DO SISTEMA *'.
      *----------------------------------------------------------------*

       01  WRK-BRAD7600.
           03  WRK-B7600-DT-JULIANA      PIC 9(05) COMP-3 VALUE ZEROS.
           03  WRK-B7600-DT-AAMMDD       PIC 9(07) COMP-3 VALUE ZEROS.
           03  WRK-B7600-DT-AAAAMMDD     PIC 9(08) COMP-3 VALUE ZEROS.
           03  WRK-B7600-TI-HHMMSS       PIC 9(07) COMP-3 VALUE ZEROS.
           03  WRK-B7600-TI-HHMMSSMMMMMM PIC 9(13) COMP-3 VALUE ZEROS.
           03  WRK-B7600-TIMESTAMP       PIC X(20)        VALUE SPACES.

      *----------------------------------------------------------------*
      *   AREA DE INCLUDE DB2                                          *
      *----------------------------------------------------------------*

CAT408*---------------------------------------------------------------*
CAT408 01  FILLER                      PIC X(032)          VALUE
CAT408     '*** INCLUDE RUECB099         ***'.
CAT408*---------------------------------------------------------------*
CAT408
CAT408     EXEC SQL
CAT408         INCLUDE RUECB099
CAT408     END-EXEC.
CAT408
CAT408*---------------------------------------------------------------*
CAT408 01  FILLER                      PIC X(032)          VALUE
CAT408     '*** DEMAIS TABELAS           ***'.
CAT408*---------------------------------------------------------------*
CAT408
           EXEC SQL
               INCLUDE RUECB09A
           END-EXEC.

           EXEC SQL
               INCLUDE RUECB09B
           END-EXEC.

           EXEC SQL
               INCLUDE RUFIB002
           END-EXEC.

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** RUECC890 - FIM DA AREA DE WORKING ***'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       LINKAGE                         SECTION.
      *---------------------------------------------------------------*

       01  LNK-PARM.
           05  LNK-TAMANHO   PIC S9(04) COMP.
           05  LNK-PARAMETRO PIC  X(05).

      *===============================================================*
       PROCEDURE                       DIVISION USING LNK-PARM.
      *===============================================================*

      *---------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *---------------------------------------------------------------*

           CALL 'CKRS1000'.

           PERFORM 0100-INICIALIZAR-CKRS0100.

           PERFORM 1000-INICIALIZAR.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-FS-ARQLIBER     EQUAL '10'.

           PERFORM 9000-FINALIZAR.

           PERFORM 0140-FINALIZAR-CKRS0100.

           STOP RUN.

      *---------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ROTINA DE INICIALIZACAO DO MODULO CKRS0100                    *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-CKRS0100       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE CKRS01-INTERFACE.

           MOVE  SPACES                    TO  CK01-PLAN.
           MOVE  'DB2'                     TO  CK01-ID-DB2.
           MOVE  'I'                       TO  CK01-FUNCAO.
           MOVE  LENGTH                    OF  WRK-AREA-RESTART
                                           TO  CK01-TAM-AREA-RESTART.
           MOVE  WRK-AREA-RESTART          TO  CK01-AREA-RESTART.

           PERFORM 0110-CHAMAR-CKRS01.

           IF  CK01-STATUS              EQUAL  'REST'
               MOVE  CK01-AREA-RESTART(1:CK01-TAM-AREA-RESTART)
                                           TO  WRK-AREA-RESTART
           END-IF.

      *----------------------------------------------------------------*
       0100-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ROTINA DE CHAMADA DO MODULO CKRS0100                          *
      *----------------------------------------------------------------*
       0110-CHAMAR-CKRS01                  SECTION.
      *----------------------------------------------------------------*

           CALL  WRK-CKRS0100           USING  CKRS01-INTERFACE.

           IF  CK01-CODIGO-RETORNO  NOT EQUAL  ZEROS
               PERFORM 0120-TRATAR-ERRO-CKRS0100
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0110-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ROTINA DE TRATAMENTO DE ERRO NO ACESSO AO MODULO CKRS0100     *
      *----------------------------------------------------------------*
       0120-TRATAR-ERRO-CKRS0100       SECTION.
      *----------------------------------------------------------------*

           MOVE 'APL'                          TO  ERR-TIPO-ACESSO.

           IF  CK01-CODIGO-RETORNO          EQUAL  8
               EVALUATE CK01-CODIGO-MENSAGEM
                   WHEN 'CKRS0001'
                        MOVE 'CKRS0100 - CODIGO DE FUNCAO INCORRETO'
                                               TO  ERR-TEXTO
                   WHEN 'CKRS0002'
                        MOVE 'CKRS0100 - JA FOI FEITA UMA CHAMADA EM MOD
      -                      'O INITIALIZE'
                                               TO  ERR-TEXTO
                   WHEN 'CKRS0003'
                        MOVE 'CKRS0100 - NAO FOI EFETUADA CHAMADA EM MOD
      -                      'O INITIALIZE'
                                               TO  ERR-TEXTO
                   WHEN 'CKRS0004'
                        MOVE 'CKRS0100 - CAMPO DB2-ID DEVE ESTAR INFORMA
      -                      'DO'              TO  ERR-TEXTO
                   WHEN 'CKRS0005'
                        MOVE 'CKRS0100 - O PROCESSO NAO SE ENCONTRA CADA
      -                      'STRADO'          TO  ERR-TEXTO
                   WHEN OTHER
                        MOVE 'CKRS0100 - ERRO NAO IDENTIFICADO'
                                               TO  ERR-TEXTO
               END-EVALUATE
           ELSE
               MOVE 'CKRS0100 - ERRO GRAVE - RETURN <> DE ZEROS E 8'
                                               TO  ERR-TEXTO
           END-IF.

           DISPLAY '************** RUECC890 **************'
           DISPLAY '*                                    *'
           DISPLAY '* CK01-CODIGO-RETORNO : '
                      CK01-CODIGO-RETORNO    '           *'
           DISPLAY '* CK01-CODIGO-MENSAGEM: '
                      CK01-CODIGO-MENSAGEM         '     *'
           DISPLAY '*                                    *'
           DISPLAY '************** RUECC890 **************'.

      *----------------------------------------------------------------*
       0120-99-FIM.   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ROTINA DE ATUALIZACAO DO MODULO CKRS0100                      *
      *----------------------------------------------------------------*
       0130-ATUALIZAR-CKRS0100             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE CKRS01-INTERFACE.

           MOVE  'P'                       TO  CK01-FUNCAO.
           MOVE  'DB2 '                    TO  CK01-ID-DB2.

           MOVE  LENGTH                    OF  WRK-AREA-RESTART
                                           TO  CK01-TAM-AREA-RESTART.
           MOVE  WRK-AREA-RESTART          TO  CK01-AREA-RESTART.

           PERFORM 0110-CHAMAR-CKRS01.

      *----------------------------------------------------------------*
       0130-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ROTINA DE FINALIZACAO DO MODULO CKRS0100                      *
      *----------------------------------------------------------------*
       0140-FINALIZAR-CKRS0100             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE CKRS01-INTERFACE.

           MOVE  'F'                       TO  CK01-FUNCAO.
           MOVE  'DB2'                     TO  CK01-ID-DB2.

           PERFORM 0110-CHAMAR-CKRS01.

      *----------------------------------------------------------------*
       0140-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *    PARAMETRO 'TESTE' SERA USADO APENAS EM DESENVOLVIMENTO     *
      *---------------------------------------------------------------*
           IF  LNK-TAMANHO             EQUAL  5  AND
               LNK-PARAMETRO           EQUAL  'TESTE'
               MOVE '01.01.0001'       TO  WRK-DT-VERSAO-CATALOGO
           ELSE
               MOVE '26.02.2016'       TO  WRK-DT-VERSAO-CATALOGO
           END-IF.

           CALL 'BRAD7600'             USING WRK-BRAD7600.

           OPEN INPUT  ARQLIBER
                       ARQNCOR1
                OUTPUT ARQCOR1A.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           PERFORM 2000-LER-ARQLIBER.

           PERFORM 2100-LER-ARQNCOR1.

      *---------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *---------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-ARQLIBER.

           PERFORM 1120-TESTAR-FS-ARQNCOR1.

           PERFORM 1130-TESTAR-FS-ARQCOR1A.

      *---------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1110-TESTAR-FS-ARQLIBER         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-ARQLIBER         NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'ARQLIBER'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-ARQLIBER   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM  9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1120-TESTAR-FS-ARQNCOR1         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-ARQNCOR1         NOT EQUAL ZEROS AND '04'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'ARQNCOR1'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-ARQNCOR1   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM  9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1130-TESTAR-FS-ARQCOR1A         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-ARQCOR1A         NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'ARQCOR1A'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-ARQCOR1A   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-ARQUIVO  TO ERR-TEXTO
               PERFORM  9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2000-LER-ARQLIBER               SECTION.
      *---------------------------------------------------------------*

CAT408     READ ARQLIBER               INTO WRK-REG-ARQLIBER.

           IF WRK-FS-ARQLIBER          EQUAL '10'
              MOVE HIGH-VALUES         TO WRK-CHAVE-ARQLIBER
              GO                       TO 2000-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-ARQLIBER.

           MOVE WRK-LIB-CCONTR         TO WRK-CHV-LIB-CCONTR.

           ADD 1                       TO ACU-LIDOS-ARQLIBER.

      *---------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2100-LER-ARQNCOR1               SECTION.
      *---------------------------------------------------------------*

           READ ARQNCOR1.

           IF WRK-FS-ARQNCOR1          EQUAL '10'
              MOVE HIGH-VALUES         TO WRK-CHAVE-ARQNCOR1
              GO                       TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-ARQNCOR1.

           MOVE RUECWCOR-CCONTR-CREDT-RUECL
                                       TO WRK-CHV-NC1-CCONTR.

           ADD 1                       TO ACU-LIDOS-ARQNCOR1.

      *---------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *---------------------------------------------------------------*

           INITIALIZE  WRK-MENSAGEM

           PERFORM 3200-ACESSAR-RUECB09A

           IF  SQLCODE                 EQUAL ZEROS
               MOVE WCONTD-MSGEM-RURAL-TEXT
                                       OF RUECB09A
                                       TO WRK-MENSAGEM

CAT408         MOVE WCONTD-MSGEM-RURAL-LEN
CAT408                                 OF RUECB09A
CAT408                                 TO WRK-SIS-TAM
               PERFORM 3100-MONTAR-GRAVAR-COR0001A
           ELSE
               PERFORM 3250-VE-SE-VERSAO-ANTERIOR
               IF  SQLCODE             EQUAL ZEROS
                   ADD 1               TO ACU-DESPR-VERS-ANT
               ELSE
                   PERFORM 2100-LER-ARQNCOR1 UNTIL WRK-CHAVE-ARQNCOR1
                                       NOT LESS WRK-CHAVE-ARQLIBER
                   IF  WRK-CHAVE-ARQNCOR1
                                       EQUAL WRK-CHAVE-ARQLIBER
                       MOVE RUECWCOR-WCONTD-MSGEM-RUECL-T
                            (1:RUECWCOR-WCONTD-MSGEM-RUECL-L)
                                       TO WRK-MENSAGEM
CAT408                 MOVE RUECWCOR-WCONTD-MSGEM-RUECL-L
CAT408                                 TO WRK-SIS-TAM
                       PERFORM 3100-MONTAR-GRAVAR-COR0001A
                   ELSE
                       DISPLAY '*RUECC890* CONTRATO ' WRK-LIB-CCONTR
                               ' MENSAGEM COR0001 NAO ENCONTRADA'
                   END-IF
               END-IF
           END-IF.

           PERFORM 0130-ATUALIZAR-CKRS0100.

           PERFORM 2000-LER-ARQLIBER.

      *---------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3100-MONTAR-GRAVAR-COR0001A            SECTION.
      *---------------------------------------------------------------*
CAT408
CAT408     PERFORM 4110-TRATAR-VERSAO-CATALOGO.
CAT408     IF  WRK-ARQ-CCONTR-X        NOT NUMERIC
CAT408         MOVE WRK-MENSAGEM       TO WRK-SIS-TXT
CAT408         GO TO 3100-01-PULA-LIBERACAO
CAT408     END-IF.
CAT408
      *---------------------------------------------------------------*
      *  LOCALIZACAO DO INICIO DO GRUPO LIBERACAO
      *---------------------------------------------------------------*
           PERFORM VARYING WRK-POSICAO FROM 1 BY 1
                     UNTIL WRK-POSICAO GREATER 32495
                        OR WRK-MENSAGEM (WRK-POSICAO:6) EQUAL '{2600}'
           END-PERFORM.

           IF WRK-POSICAO              GREATER 32495
              DISPLAY '*RUECC890* CONTRATO ' WRK-LIB-CCONTR
                      ' NAO POSSUI GRUPO-COR0001-LIBRC'
              GO TO  3100-99-FIM
           END-IF.

      *---------------------------------------------------------------*
      *  LOCALIZACAO DO FINAL DO GRUPO LIBERACAO
      *---------------------------------------------------------------*
BR0618*   -PERFORM VARYING WRK-FIM-LIB FROM WRK-POSICAO
BR0618*      -BY LENGTH OF RUEC66-GRUPO-COR0001-LIBRC
BR0618*             -UNTIL WRK-FIM-LIB GREATER 32495
BR0618*            -OR WRK-MENSAGEM (WRK-FIM-LIB:6) NOT EQUAL '{2600}'
BR0618*   -END-PERFORM.

BR0618*   -IF WRK-FIM-LIB              GREATER 32495
BR0618*      -DISPLAY '*RUECC890* CONTRATO ' WRK-LIB-CCONTR
BR0618*               ' NAO LOCALIZOU FIM GRUPO-COR0001-LIBRC'
BR0618*      -GO TO  3100-99-FIM
BR0618*   -END-IF.

      *---------------------------------------------------------------*
      *  MONTAGEM DA MENSAGEM COM AS LIBERACOES ATUALIZADAS
      *  1.A PARTE
      *---------------------------------------------------------------*
           COMPUTE WRK-SIS-TAM = WRK-POSICAO - 1
           MOVE WRK-MENSAGEM (1:WRK-SIS-TAM) TO WRK-SIS-TXT
      *---------------------------------------------------------------*
      *  2.A PARTE (GRUPOS LIBERACOES)
      *---------------------------------------------------------------*
BR0618*   -PERFORM VARYING WRK-IND     FROM 1 BY 1
BR0618*             -UNTIL WRK-IND     GREATER WRK-LIB-IND
BR0618*      -MOVE WRK-LIB-DATA (WRK-IND)
BR0618*                               -TO RUEC66-DTLIBRC-RECUR
BR0618*      -MOVE ALL '0'             TO RUEC66-VLR-LIBRC-RECUR
BR0618*      -MOVE WRK-LIB-VALOR (WRK-IND)
BR0618*                               -TO RUEC66-VLR-LIBRC-RECUR (5:15)
BR0618*      -MOVE RUEC66-GRUPO-COR0001-LIBRC TO WRK-SIS-TXT
BR0618*          -(WRK-POSICAO:LENGTH OF RUEC66-GRUPO-COR0001-LIBRC)
BR0618*      -ADD LENGTH OF RUEC66-GRUPO-COR0001-LIBRC TO WRK-POSICAO
BR0618*                                                   WRK-SIS-TAM
BR0618*   -END-PERFORM.
      *---------------------------------------------------------------*
      *  NO CASO DE NAO HAVER NENHUMA LIBERACAO PARA ENVIAR
      *---------------------------------------------------------------*
BR0618*   -IF WRK-LIB-IND              EQUAL ZEROS
BR0618*      -MOVE ALL '*'             TO RUEC66-DTLIBRC-RECUR
BR0618*                                  -RUEC66-VLR-LIBRC-RECUR
BR0618*      -MOVE RUEC66-GRUPO-COR0001-LIBRC TO WRK-SIS-TXT
BR0618*          -(WRK-POSICAO:LENGTH OF RUEC66-GRUPO-COR0001-LIBRC)
BR0618*      -ADD LENGTH OF RUEC66-GRUPO-COR0001-LIBRC TO WRK-POSICAO
BR0618*                                                   WRK-SIS-TAM
BR0618*   -END-IF.

      *---------------------------------------------------------------*
      *  3.A PARTE (FINAL DA MENSAGEM)
      *---------------------------------------------------------------*
CAT408     MOVE WRK-MENSAGEM (WRK-FIM-LIB:LENGTH OF WRK-MENSAGEM
CAT408                                  - ( WRK-FIM-LIB - 1   ))
                                       TO WRK-SIS-TXT
                             (WRK-POSICAO:LENGTH OF WRK-MENSAGEM
                                        - ( WRK-POSICAO - 1   ))

           ADD  LENGTH OF WRK-MENSAGEM TO WRK-SIS-TAM

           SUBTRACT  WRK-POSICAO       FROM  WRK-SIS-TAM.

           SUBTRACT  1                 FROM  WRK-SIS-TAM.

CAT408*----------------------*
CAT408 3100-01-PULA-LIBERACAO.
CAT408*----------------------*
CAT408
           PERFORM 3300-ACESSAR-RUFIB002.

           IF SQLCODE                  NOT EQUAL ZEROS
              DISPLAY '*RUECC890* CONTRATO ' WRK-LIB-CCONTR
                      ' NAO ENCONTRADO NA RUFIB002'
              GO TO  3100-99-FIM
           END-IF.

      *---------------------------------------------------------------*
      *  SOBREPOE RUEC66-NUMREF-BCCOROR DA MENSAGEM ORIGINAL
      *---------------------------------------------------------------*
           MOVE WRK-SIS-TXT (1:LENGTH OF RUEC66-COR0001-1)
                                         TO RUEC66-COR0001-1
           MOVE CREFT-RECOR OF RUFIB002  TO RUEC66-NUMREF-BCCOROR
           MOVE RUEC66-COR0001-1         TO
                WRK-SIS-TXT (1:LENGTH OF RUEC66-COR0001-1).

CAT408*---------------------------------------------------------------*
CAT408*    TRATA GRUPO DE DESCLASSIFICACAO
CAT408*---------------------------------------------------------------*
CAT408
CAT408     IF  WRK-DES-CCONTR-X        NUMERIC
CAT408         MOVE WRK-SIS-TXT        TO WRK-MENSAGEM
CAT408         PERFORM 4120-EXCLUIR-GRUPO-DESCL
BR0618*       -MOVE WRK-DES-GRUPO (1:LENGTH OF RUEC66-COR0001-16)
BR0618*                               -TO RUEC66-COR0001-16
CAT408         PERFORM 4123-INCLUIR-GRUPO-DESCL-ARQ
CAT408         MOVE WRK-MENSAGEM       TO WRK-SIS-TXT
CAT408     END-IF.
CAT408
           PERFORM 3900-GRAVAR-ARQCOR1A.

           PERFORM 4000-ENVIA-RCOR5005.

      *---------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3200-ACESSAR-RUECB09A           SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-LIB-CCONTR         TO CCONTR-CREDT-RURAL
                                       OF RUECB09A.
CAT408     MOVE SPACES                 TO WCONTD-MSGEM-RURAL-TEXT.
CAT408
           EXEC SQL
              SELECT CCONTR_CREDT_RURAL
                   , NENVIO_RECOR
                   , NREG_MSGEM_RECOR
                   , WCONTD_MSGEM_RURAL
              INTO   :RUECB09A.CCONTR-CREDT-RURAL
                   , :RUECB09A.NENVIO-RECOR
                   , :RUECB09A.NREG-MSGEM-RECOR
                   , :RUECB09A.WCONTD-MSGEM-RURAL
              FROM   DB2PRD.TENVIO_RECOR_TEMPR
              WHERE  CCONTR_CREDT_RURAL = :RUECB09A.CCONTR-CREDT-RURAL
                AND  DATE (HINCL)       > :WRK-DT-VERSAO-CATALOGO
CAT408          AND  SUBSTR(WCONTD_MSGEM_RURAL, 1, 8 ) = 'COR0001 '
            ORDER BY NENVIO_RECOR     ASC
                   , NREG_MSGEM_RECOR DESC
              FETCH FIRST 1 ROW ONLY
           END-EXEC.

           IF (SQLCODE                NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0               EQUAL 'W'               )
               MOVE 'DB2'             TO ERR-TIPO-ACESSO
               MOVE 'TENVIO_RECOR_TEMPR' TO ERR-DBD-TAB
               MOVE 'SELECT  '        TO ERR-FUN-COMANDO
               MOVE SQLCODE           TO ERR-SQL-CODE
               MOVE '0010'            TO ERR-LOCAL
               MOVE SPACES            TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3250-VE-SE-VERSAO-ANTERIOR      SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *  VERIFICA SE EXISTEM MENSAGENS ENVIADAS, INDEPENDENTE DA DATA.
      *  OU SEJA, ANTES DA VERSAO ATUAL DO CATALOGO.
      *  SE EXISTIR, A LIBERACAO DEVERA SER DESPREZADA.
      *---------------------------------------------------------------*
           MOVE WRK-LIB-CCONTR         TO CCONTR-CREDT-RURAL
                                       OF RUECB09A.

           EXEC SQL
              SELECT CCONTR_CREDT_RURAL
              INTO   :RUECB09A.CCONTR-CREDT-RURAL
              FROM   DB2PRD.TENVIO_RECOR_TEMPR
              WHERE  CCONTR_CREDT_RURAL = :RUECB09A.CCONTR-CREDT-RURAL
              FETCH FIRST 1 ROW ONLY
           END-EXEC.

           IF (SQLCODE                NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0               EQUAL 'W'               )
               MOVE 'DB2'             TO ERR-TIPO-ACESSO
               MOVE 'TENVIO_RECOR_TEMPR' TO ERR-DBD-TAB
               MOVE 'SELECT  '        TO ERR-FUN-COMANDO
               MOVE SQLCODE           TO ERR-SQL-CODE
               MOVE '0020'            TO ERR-LOCAL
               MOVE SPACES            TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3250-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3300-ACESSAR-RUFIB002           SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-LIB-CCONTR         TO CCONTR
                                       OF RUFIB002.

           EXEC SQL
              SELECT CREFT_RECOR
              INTO   :RUFIB002.CREFT-RECOR
              FROM   DB2PRD.CONTRATO_RURAL
              WHERE  CCONTR = :RUFIB002.CCONTR
           END-EXEC.

           IF (SQLCODE                NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0               EQUAL 'W'               )
               MOVE 'DB2'             TO ERR-TIPO-ACESSO
               MOVE 'CONTRATO_RURAL'  TO ERR-DBD-TAB
               MOVE 'SELECT  '        TO ERR-FUN-COMANDO
               MOVE SQLCODE           TO ERR-SQL-CODE
               MOVE '0030'            TO ERR-LOCAL
               MOVE SPACES            TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3900-GRAVAR-ARQCOR1A            SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-LIB-CCONTR         TO WRK-SIS-CCONTR

           WRITE FD-ARQCOR1A           FROM WRK-ARQCOR1A.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1130-TESTAR-FS-ARQCOR1A.

           ADD 1                       TO ACU-GRAVS-ARQCOR1A.

      *---------------------------------------------------------------*
       3900-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4000-ENVIA-RCOR5005             SECTION.
      *---------------------------------------------------------------*

CAT408     MOVE WRK-SIS-TXT            TO WRK-MENSAGEM.
CAT408     PERFORM 4121-ALTERAR-DT-MOVTO.
CAT408     MOVE WRK-MENSAGEM             TO WRK-SIS-TXT
CAT408     COMPUTE WRK-SIS-TAM
CAT408       EQUAL WRK-FIM-LIB - 1
CAT408           + LENGTH OF RUEC66-COR0001-17
CAT408     MOVE SPACES                 TO RCOR10-SISMSG
CAT408
           INITIALIZE  RCOR10-REGISTRO.

           MOVE 'N' TO  RCOR10-IND-CONTINUA

           MOVE  'RUEC'                  TO RCOR10-CCUSTO.

           MOVE WRK-SIS-TAM              TO RCOR10-TAM-SISMSG.
           MOVE WRK-SIS-TXT (1:WRK-SIS-TAM)
                                         TO RCOR10-SISMSG.
           MOVE 1                        TO RCOR10-SEQ-MSG.

           CALL WRK-MODULO-RCOR5005  USING  RCOR10-REGISTRO
                                            ERRO-AREA
                                            SQLCA.

           IF RCOR10-COD-RETORNO EQUAL ZEROS
              PERFORM 4100-INSERT-B09B
              ADD  1                    TO ACU-RCOR-OK
           ELSE
              DISPLAY '*RUECC890* CONTRATO ' WRK-LIB-CCONTR
                      ' ERRO RCOR5005 - COD-RET: ' RCOR10-COD-RETORNO
              ADD  1                    TO ACU-RCOR-NOK
           END-IF.

      *---------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4100-INSERT-B09B                SECTION.
      *---------------------------------------------------------------*

           MOVE ZEROS              TO NENVIO-RECOR       OF RUECB09B.
           MOVE WRK-LIB-CCONTR     TO CCONTR-CREDT-RURAL OF RUECB09B.
           EXEC SQL
             SELECT  MAX (NENVIO_RECOR)
               INTO :RUECB09B.NENVIO-RECOR
             FROM  DB2PRD.THIST_ENVIO_RECOR
               WHERE CCONTR_CREDT_RURAL = :RUECB09B.CCONTR-CREDT-RURAL
           END-EXEC.

           IF (SQLCODE           NOT EQUAL ZEROS AND -305 AND -310) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'THIST_ENVIO_RECOR' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0040'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.
CAT408
CAT408     IF  SQLCODE                  NOT EQUAL ZEROS
CAT408         GO TO 4100-99-FIM
CAT408     END-IF.
CAT408
CAT408     PERFORM 4101-NOVO-REG-RUECB099
CAT408
CAT408     PERFORM 4102-NOVO-REG-RUECB09A.
CAT408
           ADD  1                  TO NENVIO-RECOR       OF RUECB09B.
           MOVE 'COR0001'          TO CMSGEM-RECOR       OF RUECB09B.
           MOVE RCOR10-NRO-CTRL-IF TO CCTRL-RQUIS-FINCR  OF RUECB09B.
           MOVE SPACES             TO CUNIC-MSGEM        OF RUECB09B.
           MOVE CREFT-RECOR OF RUFIB002
                                   TO CREFT-RECOR        OF RUECB09B.
           MOVE 2                  TO CSIT-MSGEM         OF RUECB09B.
           MOVE '999999999'        TO CUSUAR-RESP        OF RUECB09B.

           EXEC SQL
             INSERT INTO DB2PRD.THIST_ENVIO_RECOR
                   (CCONTR_CREDT_RURAL
                  , NENVIO_RECOR
                  , HULT_ATULZ
                  , CMSGEM_RECOR
                  , HINCL
                  , CCTRL_RQUIS_FINCR
                  , CUNIC_MSGEM
                  , CREFT_RECOR
                  , CSIT_MSGEM
                  , CUSUAR_RESP)
             VALUES
                   (:RUECB09B.CCONTR-CREDT-RURAL
                  , :RUECB09B.NENVIO-RECOR
                  , CURRENT TIMESTAMP
                  , :RUECB09B.CMSGEM-RECOR
                  , CURRENT TIMESTAMP
                  , :RUECB09B.CCTRL-RQUIS-FINCR
                  , :RUECB09B.CUNIC-MSGEM
                  , :RUECB09B.CREFT-RECOR
                  , :RUECB09B.CSIT-MSGEM
                  , :RUECB09B.CUSUAR-RESP)
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'THIST_ENVIO_RECOR ' TO ERR-DBD-TAB
               MOVE 'INSERT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0050'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

CAT408*---------------------------------------------------------------*
CAT408 4101-NOVO-REG-RUECB099          SECTION.
CAT408*---------------------------------------------------------------*
CAT408
CAT408     MOVE WRK-LIB-CCONTR        TO CCONTR-CREDT-RURAL OF RUECB099.
CAT408
CAT408     EXEC SQL
CAT408       SELECT MAX (NENVIO_RECOR)
CAT408       INTO  :RUECB099.NENVIO-RECOR
CAT408       FROM   DB2PRD.TCONTR_ENVIO_RECOR
CAT408       WHERE  CCONTR_CREDT_RURAL  = :RUECB099.CCONTR-CREDT-RURAL
CAT408     END-EXEC.
CAT408
CAT408     IF (SQLCODE           NOT EQUAL ZEROS AND -305 AND -310) OR
CAT408        (SQLWARN0                 EQUAL 'W')
CAT408         MOVE 'DB2'               TO ERR-TIPO-ACESSO
CAT408         MOVE 'THIST_ENVIO_RECOR' TO ERR-DBD-TAB
CAT408         MOVE 'SELECT'            TO ERR-FUN-COMANDO
CAT408         MOVE SQLCODE             TO ERR-SQL-CODE
CAT408         MOVE '410A'              TO ERR-LOCAL
CAT408         MOVE SPACES              TO ERR-SEGM
CAT408         PERFORM 9999-ROTINA-ERRO
CAT408     END-IF.
CAT408
CAT408     IF  SQLCODE                NOT EQUAL ZEROS
CAT408         MOVE ZEROS             TO NENVIO-RECOR      OF RUECB09A
CAT408         GO TO 4101-99-FIM
CAT408     END-IF
CAT408
CAT408     MOVE WRK-LIB-CCONTR        TO CCONTR-CREDT-RURAL OF RUECB099.
CAT408     MOVE NENVIO-RECOR          OF RUECB099
CAT408                                TO NENVIO-RECOR       OF RUECB09A.
CAT408
CAT408     EXEC SQL
CAT408          SELECT CCONTR_CREDT_RURAL,
CAT408                 NENVIO_RECOR,
CAT408                 CCNPJ_ENTID_RESP,
CAT408                 CFLIAL_CNPJ_ENTID,
CAT408                 CCTRL_CNPJ_ENTID,
CAT408                 CCNPJ_PARTC,
CAT408                 CFLIAL_CNPJ_PARTC,
CAT408                 CCTRL_CNPJ_PARTC,
CAT408                 CSIT_OPER_RURAL,
CAT408                 DCDULA,
CAT408                 DVCTO_CONTR,
CAT408                 CCDULA_RECOR,
CAT408                 CTPO_INSTT_CREDT,
CAT408                 VFINAN,
CAT408                 CCATEG_PROTR_RECOR,
CAT408                 CPROG_RURAL,
CAT408                 CTPO_FONTE_RECOR,
CAT408                 CMUN_REFT_BACEN,
CAT408                 CEMPTO_BACEN,
CAT408                 CSIST_PROD,
CAT408                 VPCELA_LIBRC,
CAT408                 VPCELA_REC_PPRIO,
CAT408                 PALIQT_PROAGRO,
CAT408                 PJURO_ENCAR_FINCR,
CAT408                 CTPO_ENCAR_COMPL,
CAT408                 CTSORO_NACIO,
CAT408                 PRISCO_TSORO_NACIO,
CAT408                 PRISCO_FUNDO_CNSTT,
CAT408                 QAREA_RECOR,
CAT408                 QUND_FINCD_RURAL,
CAT408                 CUND_MEDD_FINCD,
CAT408                 QUND_PROD_PROVV,
CAT408                 CUND_MEDD_PROVV,
CAT408                 DINIC_SAFRA,
CAT408                 DFIM_SAFRA,
CAT408                 CTPO_GARNT_RECOR,
CAT408                 VRECTA_BRUTA_RECOR,
CAT408                 DMOVTO_RECOR
CAT408            INTO :RUECB099.CCONTR-CREDT-RURAL,
CAT408                 :RUECB099.NENVIO-RECOR,
CAT408                 :RUECB099.CCNPJ-ENTID-RESP,
CAT408                 :RUECB099.CFLIAL-CNPJ-ENTID,
CAT408                 :RUECB099.CCTRL-CNPJ-ENTID,
CAT408                 :RUECB099.CCNPJ-PARTC,
CAT408                 :RUECB099.CFLIAL-CNPJ-PARTC,
CAT408                 :RUECB099.CCTRL-CNPJ-PARTC,
CAT408                 :RUECB099.CSIT-OPER-RURAL,
CAT408                 :RUECB099.DCDULA
CAT408                   :WRK-NULL-DCDULA,
CAT408                 :RUECB099.DVCTO-CONTR
CAT408                   :WRK-NULL-DVCTO-CONTR,
CAT408                 :RUECB099.CCDULA-RECOR,
CAT408                 :RUECB099.CTPO-INSTT-CREDT,
CAT408                 :RUECB099.VFINAN
CAT408                   :WRK-NULL-VFINAN,
CAT408                 :RUECB099.CCATEG-PROTR-RECOR
CAT408                   :WRK-NULL-CCATEG-PROTR-RECOR,
CAT408                 :RUECB099.CPROG-RURAL
CAT408                   :WRK-NULL-CPROG-RURAL,
CAT408                 :RUECB099.CTPO-FONTE-RECOR,
CAT408                 :RUECB099.CMUN-REFT-BACEN
CAT408                   :WRK-NULL-CMUN-REFT-BACEN,
CAT408                 :RUECB099.CEMPTO-BACEN,
CAT408                 :RUECB099.CSIST-PROD,
CAT408                 :RUECB099.VPCELA-LIBRC
CAT408                   :WRK-NULL-VPCELA-LIBRC,
CAT408                 :RUECB099.VPCELA-REC-PPRIO
CAT408                   :WRK-NULL-VPCELA-REC-PPRIO,
CAT408                 :RUECB099.PALIQT-PROAGRO,
CAT408                 :RUECB099.PJURO-ENCAR-FINCR,
CAT408                 :RUECB099.CTPO-ENCAR-COMPL,
CAT408                 :RUECB099.CTSORO-NACIO,
CAT408                 :RUECB099.PRISCO-TSORO-NACIO,
CAT408                 :RUECB099.PRISCO-FUNDO-CNSTT,
CAT408                 :RUECB099.QAREA-RECOR,
CAT408                 :RUECB099.QUND-FINCD-RURAL
CAT408                   :WRK-NULL-QUND-FINCD-RURAL,
CAT408                 :RUECB099.CUND-MEDD-FINCD,
CAT408                 :RUECB099.QUND-PROD-PROVV
CAT408                   :WRK-NULL-QUND-PROD-PROVV,
CAT408                 :RUECB099.CUND-MEDD-PROVV,
CAT408                 :RUECB099.DINIC-SAFRA
CAT408                   :WRK-NULL-DINIC-SAFRA,
CAT408                 :RUECB099.DFIM-SAFRA
CAT408                   :WRK-NULL-DFIM-SAFRA,
CAT408                 :RUECB099.CTPO-GARNT-RECOR
CAT408                   :WRK-NULL-CTPO-GARNT-RECOR,
CAT408                 :RUECB099.VRECTA-BRUTA-RECOR,
CAT408                 :RUECB099.DMOVTO-RECOR
CAT408            FROM DB2PRD.TCONTR_ENVIO_RECOR
CAT408           WHERE CCONTR_CREDT_RURAL = :RUECB099.CCONTR-CREDT-RURAL
CAT408             AND NENVIO_RECOR       = :RUECB099.NENVIO-RECOR
CAT408     END-EXEC.
CAT408
CAT408     IF (SQLCODE                  NOT EQUAL ZEROS AND +100 )
CAT408     OR (SQLWARN0                 EQUAL 'W')
CAT408        MOVE 'DB2'                TO ERR-TIPO-ACESSO
CAT408        MOVE 'TCONTR_ENVIO_RECO'  TO ERR-DBD-TAB
CAT408        MOVE 'SELECT'             TO ERR-FUN-COMANDO
CAT408        MOVE SQLCODE              TO ERR-SQL-CODE
CAT408        MOVE '410B'               TO ERR-LOCAL
CAT408        MOVE SPACES               TO ERR-SEGM
CAT408        PERFORM 9999-ROTINA-ERRO
CAT408     END-IF.
CAT408
CAT408     IF  SQLCODE                 EQUAL +100
CAT408         GO TO 4101-99-FIM
CAT408     END-IF.
CAT408
CAT408     IF  WRK-NULL-DCDULA EQUAL -1
CAT408         MOVE SPACES             TO DCDULA             OF RUECB099
CAT408     END-IF.
CAT408
CAT408     IF  WRK-NULL-DVCTO-CONTR EQUAL -1
CAT408         MOVE SPACES             TO DVCTO-CONTR        OF RUECB099
CAT408     END-IF.
CAT408
CAT408     IF  WRK-NULL-VFINAN EQUAL -1
CAT408         MOVE ZEROS              TO VFINAN             OF RUECB099
CAT408     END-IF.
CAT408
CAT408     IF  WRK-NULL-CCATEG-PROTR-RECOR EQUAL -1
CAT408         MOVE SPACES             TO CCATEG-PROTR-RECOR OF RUECB099
CAT408     END-IF.
CAT408
CAT408     IF  WRK-NULL-CPROG-RURAL EQUAL -1
CAT408         MOVE SPACES             TO CPROG-RURAL        OF RUECB099
CAT408     END-IF.
CAT408
CAT408     IF  WRK-NULL-CMUN-REFT-BACEN EQUAL -1
CAT408         MOVE ZEROS              TO CMUN-REFT-BACEN    OF RUECB099
CAT408     END-IF.
CAT408
CAT408     IF  WRK-NULL-VPCELA-LIBRC EQUAL -1
CAT408         MOVE ZEROS              TO VPCELA-LIBRC       OF RUECB099
CAT408     END-IF.
CAT408
CAT408     IF  WRK-NULL-VPCELA-REC-PPRIO EQUAL -1
CAT408         MOVE ZEROS              TO VPCELA-REC-PPRIO   OF RUECB099
CAT408     END-IF.
CAT408
CAT408     IF  WRK-NULL-QUND-FINCD-RURAL EQUAL -1
CAT408         MOVE ZEROS              TO QUND-FINCD-RURAL   OF RUECB099
CAT408     END-IF.
CAT408
CAT408     IF  WRK-NULL-QUND-PROD-PROVV EQUAL -1
CAT408         MOVE ZEROS              TO QUND-PROD-PROVV    OF RUECB099
CAT408     END-IF.
CAT408
CAT408     IF  WRK-NULL-DINIC-SAFRA EQUAL -1
CAT408         MOVE ZEROS              TO DINIC-SAFRA        OF RUECB099
CAT408     END-IF.
CAT408
CAT408     IF  WRK-NULL-DFIM-SAFRA EQUAL -1
CAT408         MOVE ZEROS              TO DFIM-SAFRA         OF RUECB099
CAT408     END-IF.
CAT408
CAT408     IF  WRK-NULL-CTPO-GARNT-RECOR EQUAL -1
CAT408         MOVE ZEROS              TO CTPO-GARNT-RECOR   OF RUECB099
CAT408     END-IF.
CAT408
CAT408     ADD  1                      TO NENVIO-RECOR      OF RUECB099.
CAT408
CAT408     EXEC SQL
CAT408          INSERT INTO DB2PRD.TCONTR_ENVIO_RECOR
CAT408                 (CCONTR_CREDT_RURAL,
CAT408                  NENVIO_RECOR,
CAT408                  CCNPJ_ENTID_RESP,
CAT408                  CFLIAL_CNPJ_ENTID,
CAT408                  CCTRL_CNPJ_ENTID,
CAT408                  CCNPJ_PARTC,
CAT408                  CFLIAL_CNPJ_PARTC,
CAT408                  CCTRL_CNPJ_PARTC,
CAT408                  CSIT_OPER_RURAL,
CAT408                  DCDULA,
CAT408                  DVCTO_CONTR,
CAT408                  CCDULA_RECOR,
CAT408                  CTPO_INSTT_CREDT,
CAT408                  VFINAN,
CAT408                  CCATEG_PROTR_RECOR,
CAT408                  CPROG_RURAL,
CAT408                  CTPO_FONTE_RECOR,
CAT408                  CMUN_REFT_BACEN,
CAT408                  CEMPTO_BACEN,
CAT408                  CSIST_PROD,
CAT408                  VPCELA_LIBRC,
CAT408                  VPCELA_REC_PPRIO,
CAT408                  PALIQT_PROAGRO,
CAT408                  PJURO_ENCAR_FINCR,
CAT408                  CTPO_ENCAR_COMPL,
CAT408                  CTSORO_NACIO,
CAT408                  PRISCO_TSORO_NACIO,
CAT408                  PRISCO_FUNDO_CNSTT,
CAT408                  QAREA_RECOR,
CAT408                  QUND_FINCD_RURAL,
CAT408                  CUND_MEDD_FINCD,
CAT408                  QUND_PROD_PROVV,
CAT408                  CUND_MEDD_PROVV,
CAT408                  DINIC_SAFRA,
CAT408                  DFIM_SAFRA,
CAT408                  CTPO_GARNT_RECOR,
CAT408                  VRECTA_BRUTA_RECOR,
CAT408                  DMOVTO_RECOR)
CAT408          VALUES (:RUECB099.CCONTR-CREDT-RURAL,
CAT408                  :RUECB099.NENVIO-RECOR,
CAT408                  :RUECB099.CCNPJ-ENTID-RESP,
CAT408                  :RUECB099.CFLIAL-CNPJ-ENTID,
CAT408                  :RUECB099.CCTRL-CNPJ-ENTID,
CAT408                  :RUECB099.CCNPJ-PARTC,
CAT408                  :RUECB099.CFLIAL-CNPJ-PARTC,
CAT408                  :RUECB099.CCTRL-CNPJ-PARTC,
CAT408                  :RUECB099.CSIT-OPER-RURAL,
CAT408                  :RUECB099.DCDULA
CAT408                    :WRK-NULL-DCDULA,
CAT408                  :RUECB099.DVCTO-CONTR
CAT408                    :WRK-NULL-DVCTO-CONTR,
CAT408                  :RUECB099.CCDULA-RECOR,
CAT408                  :RUECB099.CTPO-INSTT-CREDT,
CAT408                  :RUECB099.VFINAN
CAT408                    :WRK-NULL-VFINAN,
CAT408                  :RUECB099.CCATEG-PROTR-RECOR
CAT408                    :WRK-NULL-CCATEG-PROTR-RECOR,
CAT408                  :RUECB099.CPROG-RURAL
CAT408                    :WRK-NULL-CPROG-RURAL,
CAT408                  :RUECB099.CTPO-FONTE-RECOR,
CAT408                  :RUECB099.CMUN-REFT-BACEN
CAT408                    :WRK-NULL-CMUN-REFT-BACEN,
CAT408                  :RUECB099.CEMPTO-BACEN,
CAT408                  :RUECB099.CSIST-PROD,
CAT408                  :RUECB099.VPCELA-LIBRC
CAT408                    :WRK-NULL-VPCELA-LIBRC,
CAT408                  :RUECB099.VPCELA-REC-PPRIO
CAT408                    :WRK-NULL-VPCELA-REC-PPRIO,
CAT408                  :RUECB099.PALIQT-PROAGRO,
CAT408                  :RUECB099.PJURO-ENCAR-FINCR,
CAT408                  :RUECB099.CTPO-ENCAR-COMPL,
CAT408                  :RUECB099.CTSORO-NACIO,
CAT408                  :RUECB099.PRISCO-TSORO-NACIO,
CAT408                  :RUECB099.PRISCO-FUNDO-CNSTT,
CAT408                  :RUECB099.QAREA-RECOR,
CAT408                  :RUECB099.QUND-FINCD-RURAL
CAT408                    :WRK-NULL-QUND-FINCD-RURAL,
CAT408                  :RUECB099.CUND-MEDD-FINCD,
CAT408                  :RUECB099.QUND-PROD-PROVV
CAT408                    :WRK-NULL-QUND-PROD-PROVV,
CAT408                  :RUECB099.CUND-MEDD-PROVV,
CAT408                  :RUECB099.DINIC-SAFRA
CAT408                    :WRK-NULL-DINIC-SAFRA,
CAT408                  :RUECB099.DFIM-SAFRA
CAT408                    :WRK-NULL-DFIM-SAFRA,
CAT408                  :RUECB099.CTPO-GARNT-RECOR
CAT408                    :WRK-NULL-CTPO-GARNT-RECOR,
CAT408                  :RUECB099.VRECTA-BRUTA-RECOR,
CAT408                  :RUECB099.DMOVTO-RECOR)
CAT408     END-EXEC.
CAT408
CAT408     IF (SQLCODE                  NOT EQUAL ZEROS)
CAT408     OR (SQLWARN0                 EQUAL 'W')
CAT408        MOVE 'DB2'                TO ERR-TIPO-ACESSO
CAT408        MOVE 'TCONTR_ENVIO_RECO' TO ERR-DBD-TAB
CAT408        MOVE 'INSERT'             TO ERR-FUN-COMANDO
CAT408        MOVE SQLCODE              TO ERR-SQL-CODE
CAT408        MOVE '410C'               TO ERR-LOCAL
CAT408        MOVE SPACES               TO ERR-SEGM
CAT408        PERFORM 9999-ROTINA-ERRO
CAT408     END-IF.
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4101-99-FIM.                    EXIT.
CAT408*---------------------------------------------------------------*
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4102-NOVO-REG-RUECB09A          SECTION.
CAT408*---------------------------------------------------------------*
CAT408
CAT408     MOVE CCONTR-CREDT-RURAL     OF RUECB099
CAT408                                 TO CCONTR-CREDT-RURAL OF RUECB09A
CAT408
CAT408     EXEC SQL
CAT408       SELECT MAX (NENVIO_RECOR)
CAT408       INTO  :RUECB09A.NENVIO-RECOR
CAT408       FROM   DB2PRD.TENVIO_RECOR_TEMPR
CAT408       WHERE  CCONTR_CREDT_RURAL  = :RUECB09A.CCONTR-CREDT-RURAL
CAT408     END-EXEC.
CAT408
CAT408     IF (SQLCODE           NOT EQUAL ZEROS AND -305 AND -310) OR
CAT408        (SQLWARN0                 EQUAL 'W')
CAT408         MOVE 'DB2'               TO ERR-TIPO-ACESSO
CAT408         MOVE 'TENVIO_RECOR_TEMP' TO ERR-DBD-TAB
CAT408         MOVE 'SELECT'            TO ERR-FUN-COMANDO
CAT408         MOVE SQLCODE             TO ERR-SQL-CODE
CAT408         MOVE '420A'              TO ERR-LOCAL
CAT408         MOVE SPACES              TO ERR-SEGM
CAT408         PERFORM 9999-ROTINA-ERRO
CAT408     END-IF.
CAT408
CAT408     IF  SQLCODE                NOT EQUAL ZEROS
CAT408         MOVE ZEROS             TO NENVIO-RECOR      OF RUECB09A
CAT408     END-IF
CAT408
CAT408     IF  NENVIO-RECOR            OF RUECB09A
CAT408                                 EQUAL ZEROS
CAT408         GO TO 4102-99-FIM
CAT408     END-IF.
CAT408
CAT408     EXEC SQL
CAT408          SELECT CCONTR_CREDT_RURAL,
CAT408                 NENVIO_RECOR,
CAT408                 NREG_MSGEM_RECOR,
CAT408                 CSIT_MSGEM,
CAT408                 HINCL
CAT408            INTO :RUECB09A.CCONTR-CREDT-RURAL,
CAT408                 :RUECB09A.NENVIO-RECOR,
CAT408                 :RUECB09A.NREG-MSGEM-RECOR,
CAT408                 :RUECB09A.CSIT-MSGEM
CAT408                   :WRK-NULL-CSIT-MSGEM,
CAT408                 :RUECB09A.HINCL
CAT408                   :WRK-NULL-HINCL
CAT408            FROM DB2PRD.TENVIO_RECOR_TEMPR
CAT408           WHERE CCONTR_CREDT_RURAL = :RUECB09A.CCONTR-CREDT-RURAL
CAT408             AND NENVIO_RECOR       = :RUECB09A.NENVIO-RECOR
CAT408     END-EXEC.
CAT408
CAT408     IF (SQLCODE                  NOT EQUAL ZEROS AND +100)
CAT408     OR (SQLWARN0                 EQUAL 'W')
CAT408        MOVE 'DB2'                TO ERR-TIPO-ACESSO
CAT408        MOVE 'TENVIO_RECOR_TEMP' TO ERR-DBD-TAB
CAT408        MOVE 'SELECT'             TO ERR-FUN-COMANDO
CAT408        MOVE SQLCODE              TO ERR-SQL-CODE
CAT408        MOVE '410D'               TO ERR-LOCAL
CAT408        MOVE SPACES               TO ERR-SEGM
CAT408        PERFORM 9999-ROTINA-ERRO
CAT408     END-IF.
CAT408
CAT408     IF  SQLCODE                  EQUAL +100
CAT408         GO TO 4102-99-FIM
CAT408     END-IF.
CAT408
CAT408     IF  WRK-NULL-CSIT-MSGEM EQUAL -1
CAT408         MOVE ZEROS              TO CSIT-MSGEM         OF RUECB09A
CAT408     END-IF.
CAT408
CAT408     IF  WRK-NULL-HINCL EQUAL -1
CAT408         MOVE SPACES             TO HINCL              OF RUECB09A
CAT408     END-IF.
CAT408
CAT408     MOVE NENVIO-RECOR OF RUECB099 TO NENVIO-RECOR    OF RUECB09A.
CAT408
CAT408     MOVE WRK-SIS-TAM            TO WCONTD-MSGEM-RURAL-LEN
CAT408                                                      OF RUECB09A.
CAT408     MOVE RCOR10-SISMSG          TO WCONTD-MSGEM-RURAL-TEXT
CAT408                                                      OF RUECB09A.
CAT408
CAT408     EXEC SQL
CAT408          INSERT INTO DB2PRD.TENVIO_RECOR_TEMPR
CAT408                 (CCONTR_CREDT_RURAL,
CAT408                  NENVIO_RECOR,
CAT408                  NREG_MSGEM_RECOR,
CAT408                  CSIT_MSGEM,
CAT408                  HINCL,
CAT408                  WCONTD_MSGEM_RURAL)
CAT408          VALUES (:RUECB09A.CCONTR-CREDT-RURAL,
CAT408                  :RUECB09A.NENVIO-RECOR,
CAT408                  :RUECB09A.NREG-MSGEM-RECOR,
CAT408                  :RUECB09A.CSIT-MSGEM
CAT408                    :WRK-NULL-CSIT-MSGEM,
CAT408                  :RUECB09A.HINCL
CAT408                    :WRK-NULL-HINCL,
CAT408                  :RUECB09A.WCONTD-MSGEM-RURAL)
CAT408     END-EXEC.
CAT408
CAT408     IF (SQLCODE                  NOT EQUAL ZEROS AND -803) OR
CAT408        (SQLWARN0                 EQUAL 'W')
CAT408        MOVE 'DB2'                TO ERR-TIPO-ACESSO
CAT408        MOVE 'TENVIO_RECOR_TEMP'  TO ERR-DBD-TAB
CAT408        MOVE 'INSERT'             TO ERR-FUN-COMANDO
CAT408        MOVE SQLCODE              TO ERR-SQL-CODE
CAT408        MOVE '410E'               TO ERR-LOCAL
CAT408        MOVE SPACES               TO ERR-SEGM
CAT408        PERFORM 9999-ROTINA-ERRO
CAT408     END-IF.
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4102-99-FIM.                    EXIT.
CAT408*---------------------------------------------------------------*
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4110-TRATAR-VERSAO-CATALOGO     SECTION.
CAT408*---------------------------------------------------------------*
CAT408
CAT408     PERFORM 4111-CHECAR-VERSAO-407
CAT408
CAT408     IF  WRK-VERSAO-407          EQUAL 'N'
CAT408         PERFORM 4121-ALTERAR-DT-MOVTO
CAT408         GO TO 4110-99-FIM
CAT408     END-IF.
CAT408
CAT408***  CONVERTER DA VERSAO DO CATALOGO 4.07 PARA 4.08
CAT408
BR0618*   -MOVE ALL '*'                TO RUEC66-DTLIBRC-RECUR.
BR0618*   -MOVE ALL '*'                TO RUEC66-VLR-LIBRC-RECUR.
CAT408     PERFORM 4122-INCLUIR-GRUPO-DESCL-VAZIO
CAT408
CAT408     PERFORM 4121-ALTERAR-DT-MOVTO.
CAT408     PERFORM 4113-INCLUIR-COD-MUN-GLEBA.
CAT408
CAT408     PERFORM 4114-TRATAR-GRUPO-PTO-POLIG
CAT408
CAT408     PERFORM 4116-TRATAR-TIPO-BNEFC-COR.
CAT408
CAT408     PERFORM 4118-RETIRAR-CAMPOS-SUBEMPRT.
CAT408
CAT408     PERFORM 4119-RETIRAR-TIPO-CEDULA.
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4110-99-FIM.                    EXIT.
CAT408*---------------------------------------------------------------*
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4111-CHECAR-VERSAO-407          SECTION.
CAT408*---------------------------------------------------------------*
CAT408
CAT408***  MENSAGEM QUE NAO POSSUIREM GRUPO {2700} (DESCLASSIFICACAO)
CAT408***  FORAM GERADAS NA VERSAO 4.07
CAT408     MOVE 'N'                    TO WRK-VERSAO-407
CAT408
CAT408     PERFORM VARYING WRK-POSICAO FROM 1 BY 1
CAT408               UNTIL WRK-POSICAO GREATER 32495
CAT408                  OR WRK-MENSAGEM (WRK-POSICAO:6) EQUAL '{2700}'
CAT408     END-PERFORM.
CAT408
CAT408     IF  WRK-POSICAO             GREATER 32495
CAT408***     -ADD 1                   TO ACU-CONTRATOS-408
CAT408         MOVE 'S'                TO WRK-VERSAO-407
CAT408     END-IF.
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4111-99-FIM.                    EXIT.
CAT408*---------------------------------------------------------------*
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4113-INCLUIR-COD-MUN-GLEBA      SECTION.
CAT408*---------------------------------------------------------------*
CAT408***  INCLUIR CAMPO RUEC66-COD-MUN-GLEB COM ASTERISCOS
CAT408
CAT408*---------------------------------------------------------------*
CAT408*  LOCALIZACAO DO INICIO DO GRUPO PONTO POLIGONO
CAT408*---------------------------------------------------------------*
CAT408     PERFORM VARYING WRK-POSICAO FROM 1 BY 1
CAT408               UNTIL WRK-POSICAO GREATER 32495
CAT408                  OR WRK-MENSAGEM (WRK-POSICAO:6) EQUAL '{2210}'
CAT408     END-PERFORM.
CAT408
CAT408     IF  WRK-POSICAO             GREATER 32495
CAT408         GO TO 4113-99-FIM
CAT408     END-IF.
CAT408
CAT408*---------------------------------------------------------------*
CAT408*  LOCALIZACAO DO FINAL DO GRUPO PONTO POLIGONO
CAT408*---------------------------------------------------------------*
CAT408     PERFORM VARYING WRK-FIM-LIB FROM WRK-POSICAO
CAT408        BY LENGTH OF WRK-47-GRUPO-COR0001-PTPOLG
CAT408               UNTIL WRK-FIM-LIB GREATER 32495
CAT408              OR WRK-MENSAGEM (WRK-FIM-LIB:6) NOT EQUAL '{2210}'
CAT408     END-PERFORM.
CAT408
CAT408     IF WRK-FIM-LIB              GREATER 32495
CAT408        GO TO  4113-99-FIM
CAT408     END-IF.
CAT408
CAT408*---------------------------------------------------------------*
CAT408*    INCLUIR CODIGO DE MUNICIPIO 11 BYTES APOS O GRUPO
CAT408*    PONTO POLIGONO
CAT408*---------------------------------------------------------------*
BR0618*   -MOVE ALL '*'                TO RUEC66-COD-MUN-GLEB
CAT408
CAT408     SUBTRACT 1                  FROM WRK-FIM-LIB.
CAT408     ADD      LENGTH OF RUEC66-AREA-NAO-CULT
CAT408                                 TO   WRK-FIM-LIB.
CAT408
CAT408     STRING  WRK-MENSAGEM        ( 1 : WRK-FIM-LIB )
BR0618*           -RUEC66-COD-MUN-GLEB
CAT408             WRK-MENSAGEM        ( WRK-FIM-LIB + 1:
CAT408                                 ( LENGTH OF WRK-MENSAGEM
BR0618*                                --LENGTH OF RUEC66-COD-MUN-GLEB
CAT408                                 - WRK-FIM-LIB                 ))
CAT408     DELIMITED BY SIZE           INTO WRK-MENSAGEM-408.
CAT408
CAT408     MOVE WRK-MENSAGEM-408       TO  WRK-MENSAGEM.
CAT408
BR0618*   -ADD  LENGTH OF RUEC66-COD-MUN-GLEB
BR0618*                               -TO  WRK-SIS-TAM.
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4113-99-FIM.                    EXIT.
CAT408*---------------------------------------------------------------*
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4114-TRATAR-GRUPO-PTO-POLIG     SECTION.
CAT408*---------------------------------------------------------------*
CAT408***  ALTERAR QUANTIDADE DE CASAS DECIMAIS DOS CAMPOS
CAT408***  RUEC66-LAT-PONTO E RUEC66-LONG-PONTO DE 6 PARA 11
CAT408
CAT408*---------------------------------------------------------------*
CAT408*  LOCALIZACAO DO INICIO DO GRUPO PONTO POLIGONO
CAT408*---------------------------------------------------------------*
CAT408     PERFORM VARYING WRK-POSICAO FROM 1 BY 1
CAT408               UNTIL WRK-POSICAO GREATER 32495
CAT408                  OR WRK-MENSAGEM (WRK-POSICAO:6) EQUAL '{2210}'
CAT408     END-PERFORM.
CAT408
CAT408     IF  WRK-POSICAO             GREATER 32495
CAT408         GO TO 4114-99-FIM
CAT408     END-IF.
CAT408
CAT408*---------------------------------------------------------------*
CAT408*  SALVAR AREA ANTES DO INICIO DO GRUPO COORDENADAS
CAT408*---------------------------------------------------------------*
CAT408     MOVE WRK-MENSAGEM           ( 1 : WRK-POSICAO - 1)
CAT408                                 TO WRK-MENSAGEM-408.
CAT408
CAT408*---------------------------------------------------------------*
CAT408*  CONCATENAR COORDENADAS REFORMATADAS
CAT408*---------------------------------------------------------------*
CAT408     MOVE WRK-POSICAO            TO WRK-POSICAO-408.
CAT408
CAT408     PERFORM 4115-MOVER-COORDENADAS
CAT408       UNTIL WRK-POSICAO                GREATER 32495
CAT408          OR WRK-MENSAGEM(WRK-POSICAO:6) NOT EQUAL '{2210}'
CAT408
CAT408*---------------------------------------------------------------*
CAT408*  REFORMATAR AREA RESTANTE
CAT408*---------------------------------------------------------------*
CAT408     MOVE WRK-MENSAGEM ( WRK-POSICAO    : LENGTH OF WRK-MENSAGEM
CAT408                     - ( WRK-POSICAO    - 1 ))
CAT408                                 TO WRK-MENSAGEM-408
CAT408                       (WRK-POSICAO-408 : LENGTH OF WRK-MENSAGEM
CAT408                     - (WRK-POSICAO-408 - 1 )).
CAT408
CAT408     MOVE WRK-MENSAGEM-408       TO WRK-MENSAGEM.
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4114-99-FIM.                    EXIT.
CAT408*---------------------------------------------------------------*
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4115-MOVER-COORDENADAS          SECTION.
CAT408*---------------------------------------------------------------*
CAT408
CAT408     MOVE WRK-MENSAGEM           ( WRK-POSICAO :
CAT408                                   LENGTH OF WRK-47-COR0001-7 )
CAT408                                 TO WRK-47-COR0001-7
CAT408
CAT408     IF  WRK-47-LAT-PONTO(1:1)   EQUAL '*'
CAT408         MOVE ALL '*'            TO WRK-48-LAT-PONTO
CAT408     ELSE
CAT408         MOVE WRK-47-LAT-PONTO-N TO WRK-48-LAT-PONTO-N
CAT408     END-IF
CAT408
CAT408     IF  WRK-47-LONG-PONTO(1:1)  EQUAL '*'
CAT408         MOVE ALL '*'            TO WRK-48-LONG-PONTO
CAT408     ELSE
CAT408         MOVE WRK-47-LONG-PONTO-N TO WRK-48-LONG-PONTO-N
CAT408     END-IF.
CAT408
CAT408     IF  WRK-47-ALTT-PONTO(1:1)  EQUAL '*'
CAT408         MOVE ALL '*'            TO WRK-48-ALTT-PONTO
CAT408     ELSE
CAT408         MOVE WRK-47-ALTT-PONTO  TO WRK-48-ALTT-PONTO
CAT408     END-IF.
CAT408
CAT408     MOVE  WRK-48-COR0001-7      TO WRK-MENSAGEM-408 (
CAT408           WRK-POSICAO-408 : LENGTH  OF WRK-48-COR0001-7 )
CAT408
CAT408     ADD   LENGTH                OF WRK-47-COR0001-7
CAT408                                 TO WRK-POSICAO.
CAT408
CAT408     ADD   LENGTH                OF WRK-48-COR0001-7
CAT408                                 TO WRK-POSICAO-408.
CAT408
CAT408     SUBTRACT LENGTH             OF WRK-47-COR0001-7
CAT408                               FROM WRK-SIS-TAM.
CAT408
CAT408     ADD      LENGTH             OF WRK-48-COR0001-7
CAT408                                 TO WRK-SIS-TAM.
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4115-99-FIM.                    EXIT.
CAT408*---------------------------------------------------------------*
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4116-TRATAR-TIPO-BNEFC-COR      SECTION.
CAT408*---------------------------------------------------------------*
CAT408***  INCLUIR CAMPO RUEC66-TPO-BENF COM '***'
CAT408
CAT408*---------------------------------------------------------------*
CAT408*  LOCALIZACAO DO INICIO DO GRUPO PONTO EMITENTE
CAT408*---------------------------------------------------------------*
CAT408     PERFORM VARYING WRK-POSICAO FROM 1 BY 1
CAT408               UNTIL WRK-POSICAO GREATER 32495
CAT408                  OR WRK-MENSAGEM (WRK-POSICAO:6) EQUAL '{1000}'
CAT408     END-PERFORM.
CAT408
CAT408     IF  WRK-POSICAO             GREATER 32495
CAT408         GO TO 4116-99-FIM
CAT408     END-IF.
CAT408
CAT408*---------------------------------------------------------------*
CAT408*  SALVAR AREA ANTES DO INICIO DO GRUPO EMITENTE
CAT408*---------------------------------------------------------------*
CAT408     MOVE WRK-MENSAGEM           ( 1 : WRK-POSICAO - 1)
CAT408                                 TO WRK-MENSAGEM-408.
CAT408
CAT408*---------------------------------------------------------------*
CAT408*  CONCATENAR GRUPOS EMITENTES REFORMATADOS
CAT408*---------------------------------------------------------------*
CAT408     MOVE WRK-POSICAO            TO WRK-POSICAO-408.
CAT408
CAT408     PERFORM 4117-MOVER-EMITENTES
CAT408       UNTIL WRK-POSICAO                GREATER 32495
CAT408         OR WRK-MENSAGEM(WRK-POSICAO:6) NOT EQUAL '{1000}'
CAT408
CAT408*---------------------------------------------------------------*
CAT408*  REFORMATAR AREA RESTANTE
CAT408*---------------------------------------------------------------*
CAT408     MOVE WRK-MENSAGEM ( WRK-POSICAO    : LENGTH OF WRK-MENSAGEM
CAT408                     - ( WRK-POSICAO    - 1 ))
CAT408                                 TO WRK-MENSAGEM-408
CAT408                       (WRK-POSICAO-408 : LENGTH OF WRK-MENSAGEM
CAT408                     - (WRK-POSICAO-408 - 1 )).
CAT408
CAT408     MOVE WRK-MENSAGEM-408       TO WRK-MENSAGEM.
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4116-99-FIM.                    EXIT.
CAT408*---------------------------------------------------------------*
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4117-MOVER-EMITENTES            SECTION.
CAT408*---------------------------------------------------------------*
CAT408
CAT408     MOVE WRK-MENSAGEM           ( WRK-POSICAO :
CAT408                                   LENGTH OF WRK-47-COR0001-2 )
CAT408                                 TO WRK-47-COR0001-2
CAT408
CAT408     MOVE WRK-47-TP-PESSOA-EMIT  TO WRK-48-TP-PESSOA-EMIT
CAT408     MOVE WRK-47-CNPJ-CPF-EMIT   TO WRK-48-CNPJ-CPF-EMIT
CAT408     MOVE WRK-47-DAP-EMIT        TO WRK-48-DAP-EMIT
CAT408     MOVE '***'                  TO WRK-48-TPO-BENF
CAT408
CAT408     MOVE  WRK-48-COR0001-2      TO WRK-MENSAGEM-408 (
CAT408           WRK-POSICAO-408 : LENGTH  OF WRK-48-COR0001-2 )
CAT408
CAT408     ADD   LENGTH                OF WRK-47-COR0001-2
CAT408                                 TO WRK-POSICAO.
CAT408
CAT408     ADD   LENGTH                OF WRK-48-COR0001-2
CAT408                                 TO WRK-POSICAO-408.
CAT408
CAT408     SUBTRACT LENGTH             OF WRK-47-COR0001-2
CAT408                               FROM WRK-SIS-TAM.
CAT408
CAT408     ADD      LENGTH             OF WRK-48-COR0001-2
CAT408                                 TO WRK-SIS-TAM.
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4117-99-FIM.                    EXIT.
CAT408*---------------------------------------------------------------*
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4118-RETIRAR-CAMPOS-SUBEMPRT    SECTION.
CAT408*---------------------------------------------------------------*
CAT408***  2 - RETIRAR CAMPOS: RUEC66-CNPJ-BASEIF-SUBEMPRT
CAT408***                      RUEC66-NUMREF-BCCOR-SUBEMPRT
CAT408
CAT408     MOVE SPACES                 TO WRK-MENSAGEM-408
CAT408     STRING  WRK-MENSAGEM(1:104)
CAT408             WRK-MENSAGEM(124:32377)
CAT408             '                   '
CAT408     DELIMITED BY SIZE           INTO WRK-MENSAGEM-408.
CAT408     MOVE WRK-MENSAGEM-408       TO WRK-MENSAGEM.
CAT408
CAT408     SUBTRACT 19               FROM WRK-SIS-TAM.
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4118-99-FIM.                    EXIT.
CAT408*---------------------------------------------------------------*
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4119-RETIRAR-TIPO-CEDULA        SECTION.
CAT408*---------------------------------------------------------------*
CAT408***   RETIRAR CAMPO RUEC66-TP-CEDLCOR
CAT408
CAT408     MOVE SPACES                 TO WRK-MENSAGEM-408
CAT408     STRING  WRK-MENSAGEM(1:68)
CAT408             WRK-MENSAGEM(70:32431)
CAT408     DELIMITED BY SIZE           INTO WRK-MENSAGEM-408.
CAT408     MOVE WRK-MENSAGEM-408       TO WRK-MENSAGEM.
CAT408
CAT408     SUBTRACT 1                FROM WRK-SIS-TAM.
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4119-99-FIM.                    EXIT.
CAT408*---------------------------------------------------------------*
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4120-EXCLUIR-GRUPO-DESCL        SECTION.
CAT408*---------------------------------------------------------------*
CAT408***  EXCLUIR GRUPO RUEC66-COR0001-16 (DESCLASSIFICACAO)
CAT408***  COM ASTERISCOS
CAT408
CAT408*---------------------------------------------------------------*
CAT408*  LOCALIZACAO DO INICIO DO GRUPO DE DESCLASSIFICACAO
CAT408*---------------------------------------------------------------*
CAT408     PERFORM VARYING WRK-POSICAO FROM 1 BY 1
CAT408               UNTIL WRK-POSICAO GREATER 32495
CAT408                  OR WRK-MENSAGEM (WRK-POSICAO:6) EQUAL '{2700}'
CAT408     END-PERFORM.
CAT408
CAT408     IF  WRK-POSICAO             GREATER 32495
CAT408         GO TO 4120-99-FIM
CAT408     END-IF.
CAT408
CAT408*---------------------------------------------------------------*
CAT408*  LOCALIZACAO DO FINAL DO GRUPO DE DESCLASSIFICACAO
CAT408*---------------------------------------------------------------*
BR0618*   -PERFORM VARYING WRK-FIM-LIB FROM WRK-POSICAO
BR0618*      -BY LENGTH OF RUEC66-GRUPO-COR0001-DESCLASS
CAT408*******-BY LENGTH OF RUEC66-GRUPO-COR0001-LIBRC
BR0618*             -UNTIL WRK-FIM-LIB GREATER 32495
BR0618*            -OR WRK-MENSAGEM (WRK-FIM-LIB:6) NOT EQUAL '{2700}'
BR0618*   -END-PERFORM.
CAT408
BR0618*   -IF WRK-FIM-LIB              GREATER 32495
BR0618*      -GO TO 4120-99-FIM
BR0618*   -END-IF.
CAT408
CAT408*---------------------------------------------------------------*
CAT408*    EXCLUIR GRUPO DE DESCLASSIFICACAO
CAT408*---------------------------------------------------------------*
CAT408
CAT408     COMPUTE WRK-TAMANHO-DESC
CAT408       EQUAL WRK-FIM-LIB - WRK-POSICAO
CAT408
CAT408     STRING  WRK-MENSAGEM        ( 1 : WRK-POSICAO - 1 )
CAT408             WRK-MENSAGEM        ( WRK-FIM-LIB   :
CAT408                                 ( LENGTH OF WRK-MENSAGEM
CAT408                                 - WRK-FIM-LIB         ))
CAT408             WRK-BRANCOS         ( 1 : WRK-TAMANHO-DESC   )
CAT408     DELIMITED BY SIZE           INTO WRK-MENSAGEM-408.
CAT408
CAT408     MOVE WRK-MENSAGEM-408       TO  WRK-MENSAGEM.
CAT408
CAT408     SUBTRACT WRK-TAMANHO-DESC FROM WRK-SIS-TAM.
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4120-99-FIM.                    EXIT.
CAT408*---------------------------------------------------------------*
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4121-ALTERAR-DT-MOVTO           SECTION.
CAT408*---------------------------------------------------------------*
CAT408***  ALTERAR CAMPO RUEC66-DTMOVTO
CAT408
CAT408*---------------------------------------------------------------*
CAT408*  LOCALIZACAO DO INICIO DO GRUPO PONTO POLIGONO
CAT408*---------------------------------------------------------------*
CAT408     PERFORM VARYING WRK-POSICAO FROM 1 BY 1
CAT408               UNTIL WRK-POSICAO GREATER 32495
CAT408                  OR WRK-MENSAGEM (WRK-POSICAO:6) EQUAL '{2700}'
CAT408     END-PERFORM.
CAT408
CAT408     IF  WRK-POSICAO             GREATER 32495
CAT408         GO TO 4121-99-FIM
CAT408     END-IF.
CAT408
CAT408*---------------------------------------------------------------*
CAT408*  LOCALIZACAO DO FINAL DO GRUPO PONTO POLIGONO
CAT408*---------------------------------------------------------------*
BR0618*   -PERFORM VARYING WRK-FIM-LIB FROM WRK-POSICAO
BR0618*      -BY LENGTH OF RUEC66-GRUPO-COR0001-DESCLASS
BR0618*             -UNTIL WRK-FIM-LIB GREATER 32495
BR0618*            -OR WRK-MENSAGEM (WRK-FIM-LIB:6) NOT EQUAL '{2700}'
BR0618*   -END-PERFORM.
CAT408
BR0618*   -IF WRK-FIM-LIB              GREATER 32495
BR0618*      -GO TO  4121-99-FIM
BR0618*   -END-IF.
CAT408
CAT408*---------------------------------------------------------------*
CAT408*    ALTERAR DATA DE MOVIMENTO APOS O GRUPO DE DESCLASSIFICACAO
CAT408*---------------------------------------------------------------*
CAT408
CAT408     MOVE WRK-B7600-DT-AAAAMMDD  TO RUEC66-DTMOVTO
CAT408
CAT408     STRING  WRK-MENSAGEM        ( 1 : WRK-FIM-LIB - 1)
CAT408             RUEC66-COR0001-17
CAT408     DELIMITED BY SIZE           INTO WRK-MENSAGEM-408.
CAT408
CAT408     MOVE WRK-MENSAGEM-408       TO  WRK-MENSAGEM.
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4121-99-FIM.                    EXIT.
CAT408*---------------------------------------------------------------*
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4122-INCLUIR-GRUPO-DESCL-VAZIO  SECTION.
CAT408*---------------------------------------------------------------*
CAT408***  INCLUIR GRUPO RUEC66-COR0001-16 (DESCLASSIFICACAO)
CAT408***  COM ASTERISCOS
CAT408
CAT408     PERFORM 4124-LOCALIZAR-INICIO.
CAT408
CAT408     IF  WRK-POSICAO             GREATER 32495
CAT408         GO TO 4122-99-FIM
CAT408     END-IF.
CAT408
CAT408     PERFORM 4125-LOCALIZAR-FIM.
CAT408
CAT408     IF  WRK-FIM-LIB             GREATER 32495
CAT408         GO TO  4122-99-FIM
CAT408     END-IF.
CAT408
CAT408     PERFORM 4126-INCLUIR-DESCL-VAZIO.
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4122-99-FIM.                    EXIT.
CAT408*---------------------------------------------------------------*
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4123-INCLUIR-GRUPO-DESCL-ARQ    SECTION.
CAT408*---------------------------------------------------------------*
CAT408
CAT408     PERFORM 4124-LOCALIZAR-INICIO.
CAT408
CAT408     IF  WRK-POSICAO             GREATER 32495
CAT408         GO TO 4123-99-FIM
CAT408     END-IF.
CAT408
CAT408     PERFORM 4125-LOCALIZAR-FIM.
CAT408
CAT408     IF  WRK-FIM-LIB             GREATER 32495
CAT408         GO TO  4123-99-FIM
CAT408     END-IF.
CAT408
CAT408     PERFORM 4127-INCLUIR-DESCL-ARQ.
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4123-99-FIM.                    EXIT.
CAT408*---------------------------------------------------------------*
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4124-LOCALIZAR-INICIO           SECTION.
CAT408*---------------------------------------------------------------*
CAT408*---------------------------------------------------------------*
CAT408*  LOCALIZACAO DO INICIO DO GRUPO LIBERACAO
CAT408*---------------------------------------------------------------*
CAT408
CAT408     PERFORM VARYING WRK-POSICAO FROM 1 BY 1
CAT408               UNTIL WRK-POSICAO GREATER 32495
CAT408                  OR WRK-MENSAGEM (WRK-POSICAO:6) EQUAL '{2600}'
CAT408     END-PERFORM.
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4124-99-FIM.                    EXIT.
CAT408*---------------------------------------------------------------*
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4125-LOCALIZAR-FIM              SECTION.
CAT408*---------------------------------------------------------------*
CAT408
CAT408*---------------------------------------------------------------*
CAT408*  LOCALIZACAO DO FINAL DO GRUPO LIBERACAO
CAT408*---------------------------------------------------------------*
BR0618*   -PERFORM VARYING WRK-FIM-LIB FROM WRK-POSICAO
BR0618*      -BY LENGTH OF RUEC66-GRUPO-COR0001-LIBRC
BR0618*             -UNTIL WRK-FIM-LIB GREATER 32495
BR0618*            -OR WRK-MENSAGEM (WRK-FIM-LIB:6) NOT EQUAL '{2600}'
BR0618*   -END-PERFORM.
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4125-99-FIM.                    EXIT.
CAT408*---------------------------------------------------------------*
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4126-INCLUIR-DESCL-VAZIO        SECTION.
CAT408*---------------------------------------------------------------*
CAT408*---------------------------------------------------------------*
CAT408*    INCLUIR GRUPO DE DESCLASSIFICACAO APOS GRUPO LIBERACAO
CAT408*---------------------------------------------------------------*
CAT408
BR0618*   -MOVE ALL '*'                TO RUEC66-DT-DESCLASS.
BR0618*   -MOVE ALL '*'                TO RUEC66-VLR-DESCLASS.
BR0618*   -MOVE ALL '*'                TO RUEC66-MTV-DESCLASS.
CAT408
CAT408     SUBTRACT 1                  FROM WRK-FIM-LIB.
BR0618*   -ADD      LENGTH OF RUEC66-VLR-PROD-OBTD
BR0618*                                TO   WRK-FIM-LIB.
CAT408****-ADD      LENGTH OF RUEC66-ANO-MES-PLANT
CAT408****-                            TO   WRK-FIM-LIB.
CAT408****-ADD      LENGTH OF RUEC66-ANO-MES-COLHT
CAT408****-                            TO   WRK-FIM-LIB.
CAT408     STRING  WRK-MENSAGEM        ( 1 : WRK-FIM-LIB )
BR0618*            RUEC66-COR0001-16
CAT408             WRK-MENSAGEM        ( WRK-FIM-LIB + 1:
CAT408                                 ( LENGTH OF WRK-MENSAGEM
BR0618*                                - LENGTH OF RUEC66-COR0001-16
CAT408                                 - WRK-FIM-LIB                 ))
CAT408     DELIMITED BY SIZE           INTO WRK-MENSAGEM-408.
CAT408
CAT408     MOVE WRK-MENSAGEM-408       TO  WRK-MENSAGEM.
BR0618*   -ADD  LENGTH OF RUEC66-COR0001-16
BR0618*                               -TO  WRK-SIS-TAM.
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4126-99-FIM.                    EXIT.
CAT408*---------------------------------------------------------------*
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4127-INCLUIR-DESCL-ARQ          SECTION.
CAT408*---------------------------------------------------------------*
CAT408
CAT408     PERFORM VARYING WRK-POSICAO FROM 1 BY 1
CAT408               UNTIL WRK-POSICAO GREATER LENGTH OF WRK-DES-GRUPO
CAT408                  OR WRK-DES-GRUPO(WRK-POSICAO:1) EQUAL SPACES
CAT408     END-PERFORM.
CAT408
CAT408     SUBTRACT 1                  FROM WRK-POSICAO.
BR0618*   -ADD      LENGTH OF RUEC66-VLR-PROD-OBTD
BR0618*                               -TO   WRK-FIM-LIB.
CAT408****-ADD      LENGTH OF RUEC66-ANO-MES-PLANT
CAT408****-                            TO   WRK-FIM-LIB.
CAT408****-ADD      LENGTH OF RUEC66-ANO-MES-COLHT
CAT408****-                            TO   WRK-FIM-LIB.
CAT408     STRING  WRK-MENSAGEM        ( 1 : WRK-FIM-LIB - 1)
CAT408             WRK-DES-GRUPO       ( 1 : WRK-POSICAO )
CAT408             WRK-MENSAGEM        ( WRK-FIM-LIB :
CAT408                                 ( LENGTH OF WRK-MENSAGEM
CAT408                                 - WRK-POSICAO
CAT408                                 - WRK-FIM-LIB                 ))
CAT408     DELIMITED BY SIZE           INTO WRK-MENSAGEM-408.
CAT408
CAT408     MOVE WRK-MENSAGEM-408       TO  WRK-MENSAGEM.
CAT408     ADD  WRK-POSICAO            TO  WRK-SIS-TAM.
CAT408
CAT408*---------------------------------------------------------------*
CAT408 4127-99-FIM.                    EXIT.
CAT408*---------------------------------------------------------------*
CAT408
      *---------------------------------------------------------------*
       9000-FINALIZAR                  SECTION.
      *---------------------------------------------------------------*

           MOVE ACU-LIDOS-ARQLIBER TO WRK-LIDOS-ARQLIBER-E.
           MOVE ACU-LIDOS-ARQNCOR1 TO WRK-LIDOS-ARQNCOR1-E.
           MOVE ACU-DESPR-VERS-ANT TO WRK-DESPR-VERS-ANT-E.
           MOVE ACU-GRAVS-ARQCOR1A TO WRK-GRAVS-ARQCOR1A-E.
           MOVE ACU-RCOR-OK        TO WRK-RCOR-OK-E.
           MOVE ACU-RCOR-NOK       TO WRK-RCOR-NOK-E.

           DISPLAY '********************RUECC890********************'.
           DISPLAY '*                                              *'.
           DISPLAY '*   TOTAL DE LIDOS EM ARQLIBER  = '
                      WRK-LIDOS-ARQLIBER-E '  *'.
           DISPLAY '*                                              *'.
           DISPLAY '*   TOTAL DE LIDOS EM ARQNCOR1  = '
                      WRK-LIDOS-ARQNCOR1-E '  *'.
           DISPLAY '*                                              *'.
           DISPLAY '*   MSGS VERS.ANTERIOR (DESPR.) = '
                      WRK-DESPR-VERS-ANT-E '  *'.
           DISPLAY '*                                              *'.
           DISPLAY '*   TOTAL DE GRAVS. EM ARQCOR1A = '
                      WRK-GRAVS-ARQCOR1A-E '  *'.
           DISPLAY '*                                              *'.
           DISPLAY '*   TOTAL ENVIADOS RCOR OK      = '
                      WRK-RCOR-OK-E        '  *'.
           DISPLAY '*                                              *'.
           DISPLAY '*   TOTAL ENVIADOS RCOR NOK     = '
                      WRK-RCOR-NOK-E       '  *'.
           DISPLAY '*                                              *'.
           DISPLAY '********************RUECC890********************'.

           CLOSE ARQLIBER
                 ARQNCOR1
                 ARQCOR1A.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *---------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *---------------------------------------------------------------*

           MOVE 'RUECC890'             TO ERR-PGM.

           IF  ERR-TIPO-ACESSO         EQUAL  'APL'
               CALL 'BRAD7100'         USING  WRK-BATCH
                                              ERRO-AREA
           ELSE
               CALL 'BRAD7100'         USING  WRK-BATCH
                                              ERRO-AREA
                                              SQLCA
           END-IF.

           GOBACK.

      *---------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
