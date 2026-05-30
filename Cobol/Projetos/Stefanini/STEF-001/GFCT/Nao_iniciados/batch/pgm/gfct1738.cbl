      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1738.
       AUTHOR.     PROCWORK OUTSOURCING.
      *================================================================*
      *      S O N D A   P R O C W O R K   O U T S O U R C I N G       *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT1738                                     *
      *    PROGRAMADOR  : FRANCISCO                  -  SONDA PROCWORK *
      *    ANALISTA     : RODRIGO RIOS               -  SONDA PROCWORK *
      *    ANALISTA     : DELGADO                    -  GRUPO 53       *
      *    DATA         : 09/06/2008                                   *
      *                                                                *
      *    OBJETIVO     : GERAR ARQUIVO INTERFACE PARA O SISTEMA GFCT  *
      *                                                                *
      *    ARQUIVOS     :                                              *
      *      DDNAME                              INCLUDE/BOOK          *
      *      MVTOCOBR                              I#GFCT61            *
      *      ARQCGFCT                              I#GFCT00            *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *     DB2                                                        *
      *      TABLE                               INCLUDE/BOOK          *
      *      DB2PRD.PARM_DATA_PROCM                GFCTB0A1            *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      BRAD7100 - TRATAMENTO DE ERROS                            *
      *                                                                *
      *================================================================*
2108  *                      A L T E R A C A O                         *
      *----------------------------------------------------------------*
      *                                                                *
      *    ANALISTA     : WAGNER                     -  SONDA PROCWORK *
      *    DATA         : 31/12/2008                                   *
      *                                                                *
      *    OBJETIVO     : INCLUIR PARM PARA DEFINIR GFCT00-XXX-SEQ-ARQ *
      *                                                                *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       INPUT-OUTPUT                    SECTION.
      *================================================================*
       FILE-CONTROL.

           SELECT MVTOCOBR ASSIGN      TO UT-S-MVTOCOBR
                      FILE STATUS      IS WRK-FS-MVTOCOBR.

           SELECT ARQCGFCT ASSIGN      TO UT-S-ARQCGFCT
                      FILE STATUS      IS WRK-FS-ARQCGFCT.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *================================================================*
       FILE                            SECTION.
      *================================================================*

      *----------------------------------------------------------------*
      *    INPUT  :  MOVIMENTO DE COBRANCA DE FICHA CADASTRAL          *
      *              ORG.SEQUENCIAL  -  LRECL = 030                    *
      *----------------------------------------------------------------*

       FD  MVTOCOBR
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCT61'.

      *----------------------------------------------------------------*
      *    OUTPUT :  MOVIMENTO DE TARIFAS GFCT - INTERFACE PADRAO GFCT *
      *              ORG.SEQUENCIAL  -  LRECL = 300                    *
      *----------------------------------------------------------------*

       FD  ARQCGFCT
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCT00'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*   INICIO DA WORKING GFCT1738 *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*         ACUMULADORES         *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05  WRK-ACU-LIDOS-MVTO-COBR PIC  9(009) COMP-3  VALUE ZEROS.
           05  WRK-ACU-GRAVA-COBR-GFCT PIC  9(009) COMP-3  VALUE ZEROS.

           05  WRK-ACU-REG-ENVIADO     PIC  9(010) COMP-3  VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(032)          VALUE
           '* VARIAVEIS AUXILIARES         *'.
      *---------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05  WRK-BATCH               PIC  X(008)         VALUE
               'BATCH'.

           05  WRK-IND-FIM-MVTOCOBR    PIC  X(001)         VALUE SPACES.

           05  WRK-SEQ                 PIC  X(011)         VALUE SPACES.
           05  WRK-SEQ-N               REDEFINES WRK-SEQ
                                       PIC  9(011).

           05  WRK-MASCARA             PIC  ZZZZ.ZZZ.ZZ9   VALUE ZEROS.

           05  WRK-SEQ-REGISTRO        PIC  9(011) COMP-3  VALUE ZEROS.
           05  WRK-SEQ-CONTROLE        PIC  9(009) COMP-3  VALUE ZEROS.
           05  WRK-SEQ-GRUPO-EVENTO    PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*  AREA FILE-STATUS            *'.
      *----------------------------------------------------------------*

       01  WRK-FS-MVTOCOBR             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-ARQCGFCT             PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           'NA ABERTURA'.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           'NA LEITURA'.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           'NA GRAVACAO'.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*  MENSAGENS DE ERRO           *'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGEM-ERRO.
           05  FILLER                  PIC  X(007)         VALUE SPACES.
           05  FILLER                  PIC  X(007)         VALUE
               '* ERRO '.
           05  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE
              ' DO ARQUIVO '.
           05  WRK-ARQUIVO             PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(017)         VALUE
              ' - FILE-STATUS = '.
           05  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(002)         VALUE ' *'.

       01  WRK-MSG-ARQ-VAZIO.
           03  FILLER                  PIC  X(025)         VALUE SPACES.
           03  FILLER                  PIC  X(022)         VALUE
               'ARQUIVO MVTOCOBR VAZIO'.

1208   01  WRK-MSG-PARM.
           03  FILLER                  PIC  X(025)         VALUE SPACES.
           03  FILLER                  PIC  X(022)         VALUE
               'PARAMETRO INVALIDO    '.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*       AREA DA BRAD7100       *'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*          TABELAS DB2         *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A1
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*   FIM DA WORKING GFCT1738    *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
1208   LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LKG-PARAMETROS.
           03  LKG-TAMANHO             PIC S9(04)  COMP.
           03  LKG-SEQ                 PIC  X(11).

      *================================================================*
1208   PROCEDURE                       DIVISION USING LKG-PARAMETROS.
      *================================================================*

      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL                                            *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD1050'.

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 9000-FINALIZAR.

           STOP RUN.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA INICIAL DO PROGRAMA                                  *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  MVTOCOBR
                OUTPUT ARQCGFCT.

           MOVE  WRK-ABERTURA          TO WRK-OPERACAO.

           PERFORM 9200-TESTAR-FILE-STATUS.

1208       PERFORM 1100-VERIFICA-PARM.

           PERFORM 1200-VERIFICA-VAZIO.

           PERFORM 1300-OBTER-DATA-PROC.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
1208   1100-VERIFICA-PARM              SECTION.
      *----------------------------------------------------------------*

           IF  LKG-SEQ                 NOT NUMERIC
               DISPLAY '************* GFCT1738 ************'
               DISPLAY '*                                 *'
               DISPLAY '*  PARAMETRO NUM.SEQUENC.INVALIDO *'
               DISPLAY '*  LKG-SEQ        : ' LKG-SEQ
                                             '            *'
               DISPLAY '*                                 *'
               DISPLAY '************* GFCT1738 ************'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-MSG-PARM       TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     VERIFICA SE OS ARQUIVOS DE ENTRADA ESTAO VAZIOS            *
      *----------------------------------------------------------------*
       1200-VERIFICA-VAZIO             SECTION.
      *----------------------------------------------------------------*

           PERFORM 2210-LER-ARQUIVO-MVTOCOBR.

           IF  WRK-IND-FIM-MVTOCOBR    EQUAL 'S'
               DISPLAY '*************** GFCT1300 ***************'
               DISPLAY '*                                      *'
               DISPLAY '*        PROCESSAMENTO ENCERRADO       *'
               DISPLAY '*                                      *'
               DISPLAY '*--------------------------------------*'
               DISPLAY '*                                      *'
               DISPLAY '*        ARQUIVO ACRETIFD VAZIO        *'
               DISPLAY '*                                      *'
               DISPLAY '****************************************'

               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-MSG-ARQ-VAZIO  TO ERR-TEXTO

               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     OBTEM DATA DE PROCESSAMENTO DA ROTINA GFCT                 *
      *----------------------------------------------------------------*
       1300-OBTER-DATA-PROC            SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ATUAL
             INTO
                   :GFCTB0A1.DPROCM-ATUAL
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                    CSIST_PRINC         = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM'  TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0010'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM

               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAMENTO PRINCIPAL                                    *
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-GRAVA-REGISTRO-HEADER.

           PERFORM 2200-PROCESSA-MOVIMENTO-GFCT
             UNTIL WRK-IND-FIM-MVTOCOBR
                                       EQUAL 'S'.

           PERFORM 2300-GRAVA-REGISTRO-TRAILLER.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     GRAVA REGISTRO HEADER                                      *
      *----------------------------------------------------------------*
       2100-GRAVA-REGISTRO-HEADER      SECTION.
      *----------------------------------------------------------------*

           ADD  1                      TO WRK-ACU-REG-ENVIADO.

           ADD  1                      TO WRK-SEQ-REGISTRO.

           INITIALIZE GFCT00-HEADER.

1208       MOVE LKG-SEQ                TO WRK-SEQ.
1208       MOVE WRK-SEQ-N              TO GFCT00-HDR-SEQ-ARQ.

           MOVE ZEROS                  TO GFCT00-HDR-TP-REG.
           MOVE WRK-SEQ-REGISTRO       TO GFCT00-HDR-SEQ-REG.
           MOVE ZEROS                  TO GFCT00-HDR-SEQ-CTRL
                                          GFCT00-HDR-SEQ-GRP-EVN.
           MOVE 'MOVIMENTO COBRANCA RENOVACAO FICHA CADASTRAL'
                                       TO GFCT00-HDR-NOME-ARQ.
           MOVE 'FICA'                 TO GFCT00-HDR-ROT-ORIG.
           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO GFCT00-HDR-DATA-MOVTO.
           MOVE 1                      TO GFCT00-HDR-TRATAMENTO.
           MOVE 'S'                    TO GFCT00-HDR-CLIE-CRTT.
           MOVE ZEROS                  TO GFCT00-HDR-PERC-DEVOL.
           MOVE SPACES                 TO GFCT00-HDR-ERRO-FISICO
                                          GFCT00-HDR-ERRO-LOGICO.

           WRITE GFCT00-REG.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 9220-TESTAR-FS-ARQCGFCT.

           ADD  1                      TO WRK-ACU-GRAVA-COBR-GFCT.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAMENTO MOVTO GFCT ATE FIM ARQUIVO                   *
      *----------------------------------------------------------------*
       2200-PROCESSA-MOVIMENTO-GFCT    SECTION.
      *----------------------------------------------------------------*

           PERFORM 2220-GRAVA-REGISTRO-DETALHE.

           PERFORM 2210-LER-ARQUIVO-MVTOCOBR.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     LEITURA DO ARQUIVO MVTOCOBR                                *
      *----------------------------------------------------------------*
       2210-LER-ARQUIVO-MVTOCOBR       SECTION.
      *----------------------------------------------------------------*

           READ MVTOCOBR.

           IF  WRK-FS-MVTOCOBR         EQUAL '10'
               MOVE 'S'                TO WRK-IND-FIM-MVTOCOBR

               GO                      TO 2210-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 9210-TESTAR-FS-MVTOCOBR

           ADD  1                      TO WRK-ACU-LIDOS-MVTO-COBR.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     GRAVA REGISTRO DETALHE - TARIFA RENOVACAO FICHA CADASTRAL  *
      *----------------------------------------------------------------*
       2220-GRAVA-REGISTRO-DETALHE     SECTION.
      *----------------------------------------------------------------*

           ADD  1                      TO WRK-ACU-REG-ENVIADO.

           ADD  1                      TO WRK-SEQ-REGISTRO
                                          WRK-SEQ-CONTROLE
                                          WRK-SEQ-GRUPO-EVENTO.

           INITIALIZE GFCT00-DETALHE-EVN.

1208       MOVE WRK-SEQ-N              TO GFCT00-EVN-SEQ-ARQ.
1208       MOVE 1                      TO GFCT00-EVN-TP-REG.

           MOVE WRK-SEQ-REGISTRO       TO GFCT00-EVN-SEQ-REG.
           MOVE WRK-SEQ-CONTROLE       TO GFCT00-EVN-SEQ-CONTROLE.
           MOVE WRK-SEQ-GRUPO-EVENTO   TO GFCT00-EVN-SEQ-GRP-EVN.

           MOVE 9001                   TO GFCT00-EVN-COD-TARIFA.

           MOVE ZEROS                  TO GFCT00-EVN-TARIFA-MIDIA
                                          GFCT00-EVN-TARIFA-PERIF.
           MOVE 000001                 TO GFCT00-EVN-QTDE-EVENTO.
           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO GFCT00-EVN-DATA-EVENTO.

           MOVE SPACES                 TO GFCT00-EVN-HORA-EVENTO.
           MOVE ZEROS                  TO GFCT00-EVN-VALOR-EVENTO.

           MOVE 237                    TO GFCT00-EVN-BCO-ORIGEM.
           MOVE GFCT61-AGENCIA         TO GFCT00-EVN-AGE-ORIGEM.

           MOVE ZEROS                  TO GFCT00-EVN-PAB-ORIGEM.
           MOVE SPACES                 TO GFCT00-EVN-IDENT-CLIE.
           MOVE 1                      TO GFCT00-EVN-TIPO-CONTA.
           MOVE 237                    TO GFCT00-EVN-BCO-DEB-CRED.
           MOVE GFCT61-AGENCIA         TO GFCT00-EVN-AGE-DEB-CRED.

           MOVE GFCT61-CONTA           TO GFCT00-EVN-CTA-DEB-CRED.

           MOVE ZEROS                  TO GFCT00-EVN-PAB-DEB-CRED
                                          GFCT00-EVN-VALOR-PRINC.
           MOVE SPACES                 TO GFCT00-EVN-DATA-DEB-CRED.
           MOVE ZEROS                  TO GFCT00-EVN-TIPO-MOEDA.
           MOVE SPACES                 TO GFCT00-EVN-DATA-CONV.
           MOVE ZEROS                  TO GFCT00-EVN-NUM-PARCELA
                                          GFCT00-EVN-QTD-TOT-PARC
                                          GFCT00-EVN-VALOR-TOT-SERVC.
           MOVE SPACES                 TO GFCT00-EVN-ERRO-FISICO
                                          GFCT00-EVN-ERRO-LOGICO.

           WRITE GFCT00-REG.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 9220-TESTAR-FS-ARQCGFCT.

           ADD  1                      TO WRK-ACU-GRAVA-COBR-GFCT.

           MOVE ZEROS                  TO WRK-SEQ-GRUPO-EVENTO.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     GRAVA REGISTRO TRAILLER                                    *
      *----------------------------------------------------------------*
       2300-GRAVA-REGISTRO-TRAILLER    SECTION.
      *----------------------------------------------------------------*

           ADD  1                      TO WRK-ACU-REG-ENVIADO.

           ADD  1                      TO WRK-SEQ-REGISTRO
                                          WRK-SEQ-CONTROLE
                                          WRK-SEQ-GRUPO-EVENTO.

           INITIALIZE GFCT00-TRAILLER.

1208       MOVE WRK-SEQ-N              TO GFCT00-TRL-SEQ-ARQ.
           MOVE 9                      TO GFCT00-TRL-TP-REG.
           MOVE WRK-SEQ-REGISTRO       TO GFCT00-TRL-SEQ-REG.
           MOVE WRK-SEQ-CONTROLE       TO GFCT00-TRL-SEQ-CONTROLE.
           MOVE WRK-SEQ-GRUPO-EVENTO   TO GFCT00-TRL-SEQ-GRP-EVN.
           MOVE WRK-ACU-REG-ENVIADO    TO GFCT00-TRL-QTDE-REG-ENV.
           MOVE ZEROS                  TO GFCT00-TRL-VLR-TOT-PRIN
                                          GFCT00-TRL-VLR-TOT-CALC.
           MOVE SPACES                 TO GFCT00-TRL-IND-ARQ-REJ.
           MOVE ZEROS                  TO GFCT00-TRL-PERC-DEVOLVIDO
                                          GFCT00-TRL-QTDE-REG-DEV
                                          GFCT00-TRL-QTDE-EVN-RECBD
                                          GFCT00-TRL-QTDE-EVN-OK.
           MOVE SPACES                 TO GFCT00-TRL-ERRO-FISICO
                                          GFCT00-TRL-ERRO-LOGICO.

           WRITE GFCT00-REG.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 9220-TESTAR-FS-ARQCGFCT.

           ADD  1                      TO WRK-ACU-GRAVA-COBR-GFCT.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     FINALIZACAO DO PROGRAMA                                    *
      *----------------------------------------------------------------*
       9000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 9100-EMITIR-DISPLAY-TOTAIS.

           CLOSE MVTOCOBR
                 ARQCGFCT.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 9200-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     EMITIR TOTAIS DE PROCESSAMENTO
      *----------------------------------------------------------------*
       9100-EMITIR-DISPLAY-TOTAIS      SECTION.
      *----------------------------------------------------------------*

           DISPLAY '******************** GFCT1738 ********************'.
           DISPLAY '*                                                *'.
           DISPLAY '*                 TERMINO NORMAL                 *'.
           DISPLAY '*                                                *'.
           DISPLAY '*------------------------------------------------*'.
           DISPLAY '*                                                *'.

           MOVE WRK-ACU-LIDOS-MVTO-COBR
                                       TO WRK-MASCARA.

           DISPLAY '*  QUANTIDADE CERTIFICADOS LIDOS : ' WRK-MASCARA
                                                                  '  *'.

           MOVE WRK-ACU-GRAVA-COBR-GFCT
                                       TO WRK-MASCARA.

           DISPLAY '*  QUANTIDADE INTERFACE GRAVADOS : ' WRK-MASCARA
                                                                  '  *'.
           DISPLAY '*                                                *'.
           DISPLAY '**************************************************'.

      *----------------------------------------------------------------*
       9100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *      TESTE DE FILE STATUS DOS ARQUIVOS                         *
      *----------------------------------------------------------------*
       9200-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 9210-TESTAR-FS-MVTOCOBR.

           PERFORM 9220-TESTAR-FS-ARQCGFCT.

      *----------------------------------------------------------------*
       9200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTE DE FILE STATUS DO ARQUIVO MVTOCOBR                    *
      *----------------------------------------------------------------*
       9210-TESTAR-FS-MVTOCOBR         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-MVTOCOBR          NOT EQUAL '00'
              MOVE 'MVTOCOBR'          TO WRK-ARQUIVO
              MOVE WRK-FS-MVTOCOBR     TO WRK-FILE-STATUS
              MOVE WRK-MENSAGEM-ERRO   TO ERR-TEXTO
              MOVE 'APL'               TO ERR-TIPO-ACESSO

              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       9210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTE DE FILE STATUS DO ARQUIVO ARQCGFCT                    *
      *----------------------------------------------------------------*
       9220-TESTAR-FS-ARQCGFCT         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-ARQCGFCT          NOT EQUAL '00'
              MOVE 'ARQCGFCT'          TO WRK-ARQUIVO
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE WRK-FS-ARQCGFCT     TO WRK-FILE-STATUS
              MOVE WRK-MENSAGEM-ERRO   TO ERR-TEXTO

              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       9220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNO DE ERRO                                            *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1738'             TO ERR-PGM.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
