      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0888.
       AUTHOR.     DOUGLAS ROGERIO RAMOS.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0888                                    *
      *    PROGRAMADOR.:   DOUGLAS ROGERIO RAMOS   - CPM PATO BRANCO   *
      *    ANALISTA CPM:   JOSSIANE CARLA GAZZONI  - CPM PATO BRANCO   *
      *    ANALISTA....:   VALERIA                 - PROCWORK / GP. 50 *
      *    DATA........:   05/05/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   FLEXIBILIZACAO  -  DETALHAMENTO  DA         *
      *      FLEXIBILIZACAO PARA DEFERIMENTO  POR  AGRUPAMENTO.        *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                        INCLUDE/BOOK     *
      *                   DB2PRD.V01UF                  DCITV002       *
      *                   DB2PRD.MOTVO_JUSTF_OPER       GFCTB089       *
      *                   DB2PRD.PDIDO_FLEXZ_AG         GFCTB0B1       *
      *                   DB2PRD.PDIDO_FLEXZ_AGPTO      GFCTB0B2       *
      *                   DB2PRD.PDIDO_FLEXZ_CLI        GFCTB0B3       *
      *                   DB2PRD.PDIDO_FLEXZ_MUN        GFCTB0B7       *
      *                   DB2PRD.PDIDO_FLEXZ_PAB        GFCTB0B8       *
      *                   DB2PRD.PDIDO_FLEXZ_PSTAL      GFCTB0B9       *
      *                   DB2PRD.PDIDO_FLEXZ_UF         GFCTB0C1       *
      *                   DB2PRD.PDIDO_FLEXZ_SGMTO      GFCTB0C0       *
      *                   DB2PRD.SERVC_TARIF_PRINC      GFCTB0D8       *
      *                   DB2PRD.TTPO_SGMTO_GSTAO       GFCTB0H5       *
      *                   DB2PRD.TPO_OPER_MOTVO         GFCTB0G0       *
110608*                   DB2PRD.TPO_AGPTO_CTA          GFCTB0F3       *
120610*                   DB2PRD.TPDIDO_FLEXZ_RZ    GFCTB0M7       *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *    I#GFCT1V - AREA DE COMUNICACAO - ENTRADA - GFCT5006.        *
      *    I#GFCT1X - AREA DE COMUNICACAO - SAIDA   - GFCT5006.        *
      *    I#GFCTFZ - AREA DE COMUNICACAO - ENTRADA - GFCT5522.        *
      *    I#GFCTG1 - AREA DE COMUNICACAO - SAIDA   - GFCT5522.        *
      *    I#GFCTG2 - AREA DE COMUNICACAO - ENTRADA - GFCT5523.        *
      *    I#GFCTG3 - AREA DE COMUNICACAO - SAIDA   - GFCT5523.        *
      *    I#GFCTGB - AREA DE COMUNICACAO - ENTRADA - GFCT5109/5090.   *
      *    I#GFCTGC - AREA DE COMUNICACAO - SAIDA   - GFCT5109/5090.   *
      *    I#GFCTNP - AREA DE COMUNICACAO - ENTRADA.                   *
      *    I#GFCTNQ - AREA DE COMUNICACAO - SAIDA.                     *
      *    I#RURC88 - AREA DE COMUNICACAO           - RURC9020.        *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    DCIT8000 - OBTEM UF.                                        *
      *    GFCT5109 - DADOS PARA PAB'S.                                *
      *    GFCT5006 - OBTEM DESCRICAO PARA DEPENDENCIA.                *
      *    GFCT5090 - DADOS PARA CORRESPONDENTE BANCARIO.              *
      *    GFCT5522 - VERIFICAR ON-LINE.                               *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    RURC9020 - OBTEM MUNICIPIO.                                 *
      *================================================================*
      *================================================================*
      *    ULTIMA ALTERACAO EM 11/06/2008 - VALERIA                    *
      *    ALTERACOES REFERENTES AOS AGRUPAMENTOS PERSONALIZADOS       *
      *    - INCLUSAO DOS AGRUPAMENTOS PERSONALIZADOS                  *
      *    - INCLUSAO DOS CAMPOS NOVOS DE PERIODO E INICIO PERIODO     *
      *      DE COBRANCA                                               *
      *================================================================*
      *================================================================*
      *    ULTIMA ALTERACAO EM 05/2010 - LAYS                          *
      *    ACRESCENTAR LOGICA PARA BRADESCO EXPRESSO E RAZAO.          *
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
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *---------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-NOME-TAB                PIC  X(008)         VALUE SPACES.
       77  WRK-POOL7100                PIC  X(107)         VALUE SPACES.
       77  WRK-SQLCA                   PIC  X(136)         VALUE SPACES.

       01  WRK-MSG05.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA AO MODULO '.
           05 WRK-NOME-MODULO          PIC  X(008)         VALUE SPACES.

HEXA   01  WRK-COD-SQL-ERRO            PIC S9(09)     VALUE ZEROS.
       01  FILLER                      REDEFINES      WRK-COD-SQL-ERRO.
           05  FILLER                  PIC  X(006).
           05  WRK-COD-SQL-ERRO-2      PIC  9(003).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE INCLUDES *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM O MODULO GFCT5006 *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCT1V'.

       COPY 'I#GFCT1X'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM O MODULO GFCT5109/5090 *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCTGB'.

       COPY 'I#GFCTGC'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM O MODULO GFCT5522 *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCTFZ'.

       COPY 'I#GFCTG1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM O MODULO GFCT5523 *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM O MODULO RURC9020 *'.
      *---------------------------------------------------------------*

       COPY 'I#RURC88'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM O MODULO DCIT8000 *'.
      *----------------------------------------------------------------*

       01  WRK-DCIT-UNIDADE-FEDERATIVA.
           05 WRK-AREA-DCLGEN-DCITV002 PIC  X(070)         VALUE SPACES.
           05 WRK-DCIT-COD-RETORNO     PIC  9(002)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TABELA DB2 *'.
      *---------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE DCITV002
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB089
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A0
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B2
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B3
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B7
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B8
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B9
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C0
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0G0
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0H5
           END-EXEC.

110608     EXEC SQL
110608       INCLUDE GFCTB0F3
110608     END-EXEC.

120610     EXEC SQL
130610       INCLUDE GFCTB0M7
130610     END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTNP'.

       COPY 'I#GFCTNQ'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTNP-ENTRADA
                                                      GFCTNQ-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *    CHAMA AS PRINCIPAIS ROTINAS DO PROCESSAMENTO                *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCEDIMENTOS INICIAIS DO PROGRAMA                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          GFCTNQ-SAIDA.

           INITIALIZE GFCT0M-AREA-ERROS
                      GFCTNQ-SAIDA.


           PERFORM 1100-VERIFICAR-SISTEMA-DISP.

           PERFORM 1200-INICIALIZAR-AREA-SAIDA.

           PERFORM 1300-CONSISTIR-DADOS.

           PERFORM 1400-OBTER-DESC-MENSAGEM.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA A SISTUACAO DO SISTEMA.                            *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-VERIFICAR-SISTEMA-DISP     SECTION.
      *----------------------------------------------------------------*

           MOVE 100                        TO GFCTFZ-LL.
           MOVE ZEROS                      TO GFCTFZ-ZZ.
           MOVE 'GFCT'                     TO GFCTFZ-SISTEMA.
           MOVE GFCTNP-TRANSACAO           TO GFCTFZ-TRANSACAO.
           MOVE GFCTNP-FUNCAO              TO GFCTFZ-FUNCAO.
           MOVE GFCTNP-FUNC-BDSCO          TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                        TO GFCTFZ-FIM.

           MOVE 'GFCT5522'                 TO WRK-MODULO.

           CALL 'POOL0081'                 USING WRK-MODULO
                                                 GFCTFZ-ENTRADA
                                                 GFCTG1-SAIDA
                                                 GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
                                              WRK-NOME-MODULO
               MOVE WRK-MSG05              TO GFCT0M-TEXTO
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0888'             TO GFCT0M-TRANSACAO
               MOVE 9                      TO GFCTNQ-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO                 EQUAL 9
               MOVE 'GFCT0888'             TO GFCT0M-TRANSACAO
               MOVE GFCTG1-COD-SQL-ERRO    TO GFCT0M-SQLCODE
HEXA                                          WRK-COD-SQL-ERRO
.              MOVE WRK-COD-SQL-ERRO-2     TO GFCTNQ-COD-SQL-ERRO
               MOVE GFCTG1-ERRO            TO GFCTNQ-ERRO
               MOVE GFCTG1-COD-MSG-ERRO    TO GFCTNQ-COD-MSG-ERRO
               MOVE GFCTG1-DESC-MSG-ERRO   TO GFCTNQ-DESC-MSG-ERRO
                                              GFCT0M-TEXTO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE          EQUAL 1
               MOVE 9                      TO GFCTNQ-ERRO
               MOVE 'SISTEMA INDISPONIVEL' TO GFCT0M-TEXTO
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0888'             TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    INICIALIZA AREA DE SAIDA.                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-INICIALIZAR-AREA-SAIDA     SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                      GFCTNQ-SAIDA.

           MOVE 1010                       TO GFCTNQ-LL.
           MOVE GFCTNP-ZZ                  TO GFCTNQ-ZZ.
           MOVE GFCTNP-TRANSACAO           TO GFCTNQ-TRANSACAO.
           MOVE GFCTNP-FUNCAO              TO GFCTNQ-FUNCAO.

           IF GFCTNP-QTDE-OCOR             NOT NUMERIC
               MOVE ZEROS                  TO GFCTNQ-QTDE-OCOR
           ELSE
               MOVE GFCTNP-QTDE-OCOR       TO GFCTNQ-QTDE-OCOR
           END-IF.

           IF GFCTNP-QTDE-TOT-REG          NOT NUMERIC
               MOVE ZEROS                  TO GFCTNQ-QTDE-TOT-REG
           ELSE
               MOVE GFCTNP-QTDE-TOT-REG    TO GFCTNQ-QTDE-TOT-REG
           END-IF.

           MOVE GFCTNP-FUNC-BDSCO          TO GFCTNQ-FUNC-BDSCO.
           MOVE 'S'                        TO GFCTNQ-FIM.

           MOVE ZEROS                      TO GFCTNQ-ERRO
                                              GFCTNQ-COD-SQL-ERRO
                                              GFCTNQ-COD-MSG-ERRO
           MOVE '0000'                     TO GFCTG2-COD-MSG.

           PERFORM 1400-OBTER-DESC-MENSAGEM.

           MOVE GFCTG3-DESC-MSG            TO GFCTNQ-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CONSISTE OS DADOS DA INC I#GFCTNP                           *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (GFCTNP-TRANSACAO        EQUAL SPACES OR LOW-VALUES) OR
              (GFCTNP-FUNCAO           EQUAL SPACES OR LOW-VALUES) OR
              (GFCTNP-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES) OR
              (GFCTNP-AGRUPMTO         NOT NUMERIC               ) OR
110608*        GFCTNP-AGRUPMTO         NOT EQUAL 04 AND 14 AND 09  AND
110608*                                15 AND 17 AND 18 AND 19   ) OR
              (GFCTNP-TARIFA           NOT NUMERIC                 OR
               GFCTNP-TARIFA           EQUAL ZEROS)
               MOVE  1                 TO GFCTNQ-ERRO
                                          GFCTNQ-COD-MSG-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               MOVE ZEROS              TO GFCTNQ-COD-SQL-ERRO
               PERFORM 1400-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNQ-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           EVALUATE TRUE
               WHEN GFCTNP-AGRUPMTO   EQUAL 4
ST2506*            IF (GFCTNP-CGC-CPF  NOT NUMERIC   OR
ST2506*                GFCTNP-CGC-CPF  EQUAL ZEROS   OR
ST2506*                GFCTNP-FILIAL   NOT NUMERIC   OR
ST2506             IF (GFCTNP-CGC-CPF  EQUAL LOW-VALUES OR
ST2506                 GFCTNP-CGC-CPF  EQUAL SPACES OR
ST2506                 GFCTNP-FILIAL   EQUAL LOW-VALUES OR
                       GFCTNP-CONTROLE NOT NUMERIC )
                       PERFORM 1310-ERRO-CONSISTENCIA
                   END-IF
               WHEN GFCTNP-AGRUPMTO   EQUAL 9
                   IF (GFCTNP-SEGMENTO NOT NUMERIC   OR
                       GFCTNP-SEGMENTO EQUAL ZEROS  )
                       PERFORM 1310-ERRO-CONSISTENCIA
                   END-IF
               WHEN GFCTNP-AGRUPMTO   EQUAL 15 OR 17
                   IF (GFCTNP-AGENCIA  NOT NUMERIC   OR
                       GFCTNP-AGENCIA  EQUAL ZEROS   OR
                       GFCTNP-POSTO    NOT NUMERIC   OR
                       GFCTNP-POSTO    EQUAL ZEROS  )
                       PERFORM 1310-ERRO-CONSISTENCIA
                   END-IF
               WHEN GFCTNP-AGRUPMTO   EQUAL 18
                   IF (GFCTNP-MUNICIPIO NOT NUMERIC  OR
                       GFCTNP-MUNICIPIO EQUAL ZEROS )
                       PERFORM 1310-ERRO-CONSISTENCIA
                   END-IF
               WHEN GFCTNP-AGRUPMTO   EQUAL 19
                   IF (GFCTNP-UF      EQUAL SPACES )
                       PERFORM 1310-ERRO-CONSISTENCIA
                   END-IF
           END-EVALUATE.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    MOVE CAMPOS DE ERRO.                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1310-ERRO-CONSISTENCIA          SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO GFCTNQ-ERRO
                                          GFCTNQ-COD-MSG-ERRO
           MOVE '0001'                 TO GFCTG2-COD-MSG.
           MOVE ZEROS                  TO GFCTNQ-COD-SQL-ERRO.
           PERFORM 1400-OBTER-DESC-MENSAGEM.
           MOVE GFCTG3-DESC-MSG        TO GFCTNQ-DESC-MSG-ERRO.
           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM DESCRICAO DA MENSAGEM.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       1400-OBTER-DESC-MENSAGEM        SECTION.
      *----------------------------------------------------------------*

           MOVE 100                            TO GFCTG2-LL.
           MOVE ZEROS                          TO GFCTG2-ZZ.
           MOVE GFCTNP-TRANSACAO               TO GFCTG2-TRANSACAO.
           MOVE GFCTNP-FUNCAO                  TO GFCTG2-FUNCAO.
           MOVE GFCTNP-FUNC-BDSCO              TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                            TO GFCTG2-TIPO-PROC.
           MOVE 'N'                            TO GFCTG2-FIM.
           MOVE SPACES                         TO GFCTG3-SAIDA.
           INITIALIZE                          GFCTG3-SAIDA.

           MOVE 'GFCT5523'                     TO WRK-MODULO.
           CALL 'POOL0081'                     USING WRK-MODULO
                                                     GFCTG2-ENTRADA
                                                     GFCTG3-SAIDA
                                                     GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                     NOT EQUAL ZEROS
               MOVE 9                          TO GFCTNQ-ERRO
               MOVE WRK-MODULO                 TO GFCT0M-PROGRAMA
                                                  WRK-NOME-MODULO
               MOVE WRK-MSG05                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0888'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           EVALUATE GFCTG3-ERRO
               WHEN 1
                   MOVE 1                      TO GFCTNQ-ERRO
                   MOVE GFCTG3-COD-SQL-ERRO    TO GFCTNQ-COD-SQL-ERRO
                   MOVE GFCTG3-COD-MSG-ERRO    TO GFCTNQ-COD-MSG-ERRO
                   MOVE GFCTG3-DESC-MSG-ERRO   TO GFCTNQ-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               WHEN 9
                   MOVE 9                      TO GFCTNQ-ERRO
                   MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
                                                  WRK-NOME-MODULO
                   MOVE WRK-MSG05              TO GFCT0M-TEXTO
                   MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
                   MOVE 'GFCT0888'             TO GFCT0M-TRANSACAO
                   PERFORM 3000-FINALIZAR
           END-EVALUATE.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-TABELA-GFCTB0B2.

           PERFORM 2200-MOVIMENTAR-CAMPOS.

           EVALUATE GFCTNP-AGRUPMTO
               WHEN 04
                   PERFORM 2300-SELECIONAR-CLIENTE
               WHEN 09
                   PERFORM 2400-SELECIONAR-SEGMENTO
               WHEN 14
                   IF CINDCD-AGPTO-TOT OF GFCTB0B2 EQUAL 'S'
                       MOVE 'TODAS'    TO GFCTNQ-DESCR-AGPTO
                       MOVE ZEROS      TO GFCTNQ-AGENCIA
                   ELSE
                       PERFORM 2500-SELECIONAR-AGENCIA
                   END-IF
               WHEN 15
                   PERFORM 2600-SELECIONAR-PAB
               WHEN 17
                   PERFORM 2700-SELECIONAR-POSTAL
               WHEN 18
                   PERFORM 2800-SELECIONAR-MUNICIPIO
               WHEN 19
                   PERFORM 2900-SELECIONAR-UF
LFM            WHEN 20
LFM                PERFORM 2700-SELECIONAR-POSTAL
LFM            WHEN 26
LFM                PERFORM 2710-SELECIONAR-RAZAO
110608         WHEN OTHER
110608             PERFORM 2910-PESQUISA-AGRUPAMENTO
           END-EVALUATE.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    REALIZA LEITURA DA TABELA GFCTB0B2.                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-TABELA-GFCTB0B2        SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNP-AGRUPMTO        TO CAGPTO-CTA       OF GFCTB0B2.
           MOVE GFCTNP-TARIFA          TO CSERVC-TARIF     OF GFCTB0B2.
           MOVE GFCTNP-TIMESTAMP       TO HIDTFD-FLEXZ-AGPTO
                                                           OF GFCTB0B2.
           MOVE SPACES                 TO RJUSTF-SOLTC-FLEXZ-TEXT
                                                           OF GFCTB0B2.
           MOVE ZEROS                  TO RJUSTF-SOLTC-FLEXZ-LEN
                                                           OF GFCTB0B2.
           MOVE SPACES                 TO RJUSTF-PAREC-FLEXZ-TEXT
                                                           OF GFCTB0B2.
           MOVE ZEROS                  TO RJUSTF-PAREC-FLEXZ-LEN
                                                           OF GFCTB0B2.

           EXEC SQL
             SELECT
                   CAGPTO_CTA,
                   CSERVC_TARIF,
                   HIDTFD_FLEXZ_AGPTO,
                   CINDCD_AGPTO_TOT,
                   CTPO_OPER_MOTVO,
                   CMOTVO_JUSTF,
                   DINIC_FLEXZ_AGPTO,
                   DFIM_FLEXZ_AGPTO,
                   QFRANQ_FLEXZ_AGPTO,
                   CPER_FRANQ_AGPTO,
                   VNEGOC_FLEXZ_AGPTO,
                   PDESC_FLEXZ_AGPTO,
                   DDIA_AGNDA,
                   QDIA_FLEXZ_AGPTO,
                   RJUSTF_SOLTC_FLEXZ,
                   RJUSTF_PAREC_FLEXZ,
110608             CPER_COBR_TARIF,
110608             CINIC_PER_COBR
             INTO
                   :GFCTB0B2.CAGPTO-CTA,
                   :GFCTB0B2.CSERVC-TARIF,
                   :GFCTB0B2.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB0B2.CINDCD-AGPTO-TOT,
                   :GFCTB0B2.CTPO-OPER-MOTVO,
                   :GFCTB0B2.CMOTVO-JUSTF,
                   :GFCTB0B2.DINIC-FLEXZ-AGPTO,
                   :GFCTB0B2.DFIM-FLEXZ-AGPTO,
                   :GFCTB0B2.QFRANQ-FLEXZ-AGPTO,
                   :GFCTB0B2.CPER-FRANQ-AGPTO,
                   :GFCTB0B2.VNEGOC-FLEXZ-AGPTO,
                   :GFCTB0B2.PDESC-FLEXZ-AGPTO,
                   :GFCTB0B2.DDIA-AGNDA,
                   :GFCTB0B2.QDIA-FLEXZ-AGPTO,
                   :GFCTB0B2.RJUSTF-SOLTC-FLEXZ,
                   :GFCTB0B2.RJUSTF-PAREC-FLEXZ,
110608             :GFCTB0B2.CPER-COBR-TARIF,
110608             :GFCTB0B2.CINIC-PER-COBR
             FROM   DB2PRD.PDIDO_FLEXZ_AGPTO
             WHERE
                   CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0B2.CSERVC-TARIF       AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0B2.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'GFCT0888'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '          TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE             TO GFCT0M-SQLCODE
HEXA                                       WRK-COD-SQL-ERRO
.              MOVE WRK-COD-SQL-ERRO-2  TO GFCTNQ-COD-SQL-ERRO
               MOVE '0010'              TO GFCT0M-LOCAL
               MOVE SQLCA               TO GFCT0M-SQLCA-AREA
               MOVE 9                   TO GFCTNQ-ERRO
               MOVE 10                  TO GFCTNQ-COD-MSG-ERRO
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG     TO GFCTNQ-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    MOVIMENTA CAMPOS PARA INC DE SAIDA(I#GFCTNQ).               *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-MOVIMENTAR-CAMPOS          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNP-AGRUPMTO        TO GFCTNQ-AGRUPMTO.
           MOVE ZEROS                  TO GFCTNQ-CONTROLE.
           MOVE GFCTNP-TARIFA          TO GFCTNQ-TARIFA.
           MOVE GFCTNP-TIMESTAMP       TO GFCTNQ-TIMESTAMP.
           MOVE DINIC-FLEXZ-AGPTO      OF GFCTB0B2
                                       TO GFCTNQ-INIC-VIG-FLEX.
           MOVE DFIM-FLEXZ-AGPTO       OF GFCTB0B2
                                       TO GFCTNQ-FIM-VIG-FLEX.
           MOVE QFRANQ-FLEXZ-AGPTO     OF GFCTB0B2
                                       TO GFCTNQ-FRANQUIA.
           MOVE CPER-FRANQ-AGPTO       OF GFCTB0B2
                                       TO GFCTNQ-PERIODO.
           MOVE VNEGOC-FLEXZ-AGPTO     OF GFCTB0B2
                                       TO GFCTNQ-VR-NEGOCIADO.
           MOVE PDESC-FLEXZ-AGPTO      OF GFCTB0B2
                                       TO GFCTNQ-PERC-DESCTO.
           MOVE DDIA-AGNDA             OF GFCTB0B2
                                       TO GFCTNQ-DIA-AGEND.
           MOVE QDIA-FLEXZ-AGPTO       OF GFCTB0B2
                                       TO GFCTNQ-DIAS-RETENCAO.
           MOVE CTPO-OPER-MOTVO        OF GFCTB0B2
                                       TO GFCTNQ-CTIPO-OPER-MOTVO.
           MOVE CMOTVO-JUSTF           OF GFCTB0B2
                                       TO GFCTNQ-CMOTVO-JUSTF.
           MOVE ZEROS                  TO GFCTNQ-DIA-SEMANA.
           MOVE 1                      TO GFCTNQ-SEQUENCIA.

110608     MOVE CPER-COBR-TARIF        OF GFCTB0B2
110608                                 TO GFCTNQ-CPER-COBR-TARIF
110608     MOVE CINIC-PER-COBR         OF GFCTB0B2
110608                                 TO GFCTNQ-CINIC-PER-COBR.

           IF RJUSTF-PAREC-FLEXZ-TEXT  OF GFCTB0B2 EQUAL SPACES
               MOVE RJUSTF-SOLTC-FLEXZ-TEXT OF GFCTB0B2
                                            TO GFCTNQ-RJUSTF
           ELSE
               MOVE RJUSTF-PAREC-FLEXZ-TEXT OF GFCTB0B2
                                            TO GFCTNQ-RJUSTF
           END-IF.

           PERFORM 2210-PESQ-NOME-OPERACAO.
           PERFORM 2220-PESQ-NOME-MOTIVO.

           MOVE RTPO-OPER-MOTVO        OF GFCTB0G0
                                       TO GFCTNQ-NOME-OPER-MOTVO.
           MOVE RMOTVO-JUSTF           OF GFCTB089
                                       TO GFCTNQ-NOME-MOTVO.

           PERFORM 2230-PESQ-NOME-TARIFA.

           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                       TO GFCTNQ-DESCR-TARIFA.

           ADD 1                       TO GFCTNQ-QTDE-TOT-REG.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PESQUISA NOME DE OPERACAO.                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-PESQ-NOME-OPERACAO         SECTION.
      *----------------------------------------------------------------*

           MOVE CTPO-OPER-MOTVO        OF GFCTB0B2
                                       TO CTPO-OPER-MOTVO  OF GFCTB0G0.

           EXEC SQL
             SELECT
                   RTPO_OPER_MOTVO
             INTO
                   :GFCTB0G0.RTPO-OPER-MOTVO
             FROM   DB2PRD.TPO_OPER_MOTVO
             WHERE
                   CTPO_OPER_MOTVO    = :GFCTB0G0.CTPO-OPER-MOTVO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES              TO GFCT0M-ERRO-SQL
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE 'GFCT0888'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'TPO_OPER_MOTVO'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '          TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE             TO GFCT0M-SQLCODE
HEXA                                       WRK-COD-SQL-ERRO
.              MOVE WRK-COD-SQL-ERRO-2  TO GFCTNQ-COD-SQL-ERRO
               MOVE '0020'              TO GFCT0M-LOCAL
               MOVE SQLCA               TO GFCT0M-SQLCA-AREA
               MOVE 9                   TO GFCTNQ-ERRO
               MOVE 10                  TO GFCTNQ-COD-MSG-ERRO
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG     TO GFCTNQ-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PESQUISA NOME DE MOTIVO.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2220-PESQ-NOME-MOTIVO           SECTION.
      *----------------------------------------------------------------*

           MOVE CTPO-OPER-MOTVO        OF GFCTB0B2
                                       TO CTPO-OPER-MOTVO OF GFCTB089.
           MOVE CMOTVO-JUSTF           OF GFCTB0B2
                                       TO CMOTVO-JUSTF    OF GFCTB089.

           EXEC SQL
             SELECT
                   RMOTVO_JUSTF
             INTO
                   :GFCTB089.RMOTVO-JUSTF
             FROM   DB2PRD.MOTVO_JUSTF_OPER
             WHERE
                   CTPO_OPER_MOTVO    = :GFCTB089.CTPO-OPER-MOTVO    AND
                   CMOTVO_JUSTF       = :GFCTB089.CMOTVO-JUSTF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES              TO GFCT0M-ERRO-SQL
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE 'GFCT0888'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'MOTVO_JUSTF_OPER'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '          TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE             TO GFCT0M-SQLCODE
HEXA                                       WRK-COD-SQL-ERRO
.              MOVE WRK-COD-SQL-ERRO-2  TO GFCTNQ-COD-SQL-ERRO
               MOVE '0030'              TO GFCT0M-LOCAL
               MOVE SQLCA               TO GFCT0M-SQLCA-AREA
               MOVE 9                   TO GFCTNQ-ERRO
               MOVE 10                  TO GFCTNQ-COD-MSG-ERRO
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG     TO GFCTNQ-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PESQUISA NOME DE TARIFA.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2230-PESQ-NOME-TARIFA           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNP-TARIFA          TO CSERVC-TARIF     OF GFCTB0D8.

           EXEC SQL
             SELECT
                   RSERVC_TARIF_REDZD
             INTO
                   :GFCTB0D8.RSERVC-TARIF-REDZD
             FROM   DB2PRD.SERVC_TARIF_PRINC
             WHERE
                   CSERVC_TARIF       = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES              TO GFCT0M-ERRO-SQL
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE 'GFCT0888'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'SERVC_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '          TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE             TO GFCT0M-SQLCODE
HEXA                                       WRK-COD-SQL-ERRO
.              MOVE WRK-COD-SQL-ERRO-2  TO GFCTNQ-COD-SQL-ERRO
               MOVE '0040'              TO GFCT0M-LOCAL
               MOVE SQLCA               TO GFCT0M-SQLCA-AREA
               MOVE 9                   TO GFCTNQ-ERRO
               MOVE 10                  TO GFCTNQ-COD-MSG-ERRO
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG     TO GFCTNQ-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    SELECIONA CLIENTE.                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-SELECIONAR-CLIENTE         SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF          OF GFCTB0B2
                                      TO CSERVC-TARIF       OF GFCTB0B3.
           MOVE HIDTFD-FLEXZ-AGPTO    OF GFCTB0B2
                                      TO HIDTFD-FLEXZ-AGPTO OF GFCTB0B3.
           MOVE CAGPTO-CTA            OF GFCTB0B2
                                      TO CAGPTO-CTA         OF GFCTB0B3.

           EXEC SQL
ST2506*     SELECT
ST2506*           CCGC_CPF,
ST2506*           CFLIAL_CGC,
ST2506*           CCTRL_CPF_CGC
ST2506*     INTO
ST2506*           :GFCTB0B3.CCGC-CPF,
ST2506*           :GFCTB0B3.CFLIAL-CGC,
ST2506*           :GFCTB0B3.CCTRL-CPF-CGC
ST2506      SELECT
ST2506            CCGC_CPF_ST,
ST2506            CFLIAL_CGC_ST,
ST2506            CCTRL_CPF_CGC_ST
ST2506      INTO
ST2506            :GFCTB0B3.CCGC-CPF-ST,
ST2506            :GFCTB0B3.CFLIAL-CGC-ST,
ST2506            :GFCTB0B3.CCTRL-CPF-CGC-ST
             FROM   DB2PRD.PDIDO_FLEXZ_CLI
             WHERE
                   CAGPTO_CTA         = :GFCTB0B3.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0B3.CSERVC-TARIF       AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0B3.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES              TO GFCT0M-ERRO-SQL
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE 'GFCT0888'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_CLI '  TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '          TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE             TO GFCT0M-SQLCODE
HEXA                                       WRK-COD-SQL-ERRO
.              MOVE WRK-COD-SQL-ERRO-2  TO GFCTNQ-COD-SQL-ERRO
               MOVE '0050'              TO GFCT0M-LOCAL
               MOVE SQLCA               TO GFCT0M-SQLCA-AREA
               MOVE 9                   TO GFCTNQ-ERRO
               MOVE 10                  TO GFCTNQ-COD-MSG-ERRO
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG     TO GFCTNQ-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           ELSE
               IF SQLCODE              EQUAL +100
                   MOVE 1               TO GFCTNQ-ERRO
                   MOVE ZEROS           TO GFCTNQ-COD-SQL-ERRO
                   MOVE 926             TO GFCTNQ-COD-MSG-ERRO
                   MOVE '0926'          TO GFCTG2-COD-MSG
                   PERFORM 1400-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTNQ-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

           MOVE SPACES                  TO GFCTNQ-DESCR-AGPTO.
ST2506*    MOVE CCGC-CPF                OF GFCTB0B3
ST2506*                                 TO GFCTNQ-CGC-CPF.
ST2506*    MOVE CFLIAL-CGC              OF GFCTB0B3
ST2506*                                 TO GFCTNQ-FILIAL.
ST2506*    MOVE CCTRL-CPF-CGC           OF GFCTB0B3
ST2506*                                 TO GFCTNQ-CONTROLE.
ST2506     MOVE CCGC-CPF-ST             OF GFCTB0B3
ST2506                                  TO GFCTNQ-CGC-CPF.
ST2506     MOVE CFLIAL-CGC-ST           OF GFCTB0B3
ST2506                                  TO GFCTNQ-FILIAL.
ST2506     MOVE CCTRL-CPF-CGC-ST        OF GFCTB0B3
ST2506                                  TO GFCTNQ-CONTROLE.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    SELECIONA SEGMENTO.                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-SELECIONAR-SEGMENTO        SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF          OF GFCTB0B2
                                      TO CSERVC-TARIF       OF GFCTB0C0.
           MOVE HIDTFD-FLEXZ-AGPTO    OF GFCTB0B2
                                      TO HIDTFD-FLEXZ-AGPTO OF GFCTB0C0.
           MOVE CAGPTO-CTA            OF GFCTB0B2
                                      TO CAGPTO-CTA         OF GFCTB0C0.

           EXEC SQL
             SELECT
                   CSGMTO_GSTAO_TARIF
             INTO
                   :GFCTB0C0.CSGMTO-GSTAO-TARIF
             FROM   DB2PRD.PDIDO_FLEXZ_SGMTO
             WHERE
                   CAGPTO_CTA         = :GFCTB0C0.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0C0.CSERVC-TARIF       AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0C0.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES              TO GFCT0M-ERRO-SQL
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE 'GFCT0888'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_SGMTO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '          TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE             TO GFCT0M-SQLCODE
HEXA                                       WRK-COD-SQL-ERRO
.              MOVE WRK-COD-SQL-ERRO-2  TO GFCTNQ-COD-SQL-ERRO
               MOVE '0060'              TO GFCT0M-LOCAL
               MOVE SQLCA               TO GFCT0M-SQLCA-AREA
               MOVE 9                   TO GFCTNQ-ERRO
               MOVE 10                  TO GFCTNQ-COD-MSG-ERRO
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG     TO GFCTNQ-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           ELSE
               IF SQLCODE              EQUAL +100
                   MOVE 1               TO GFCTNQ-ERRO
                   MOVE ZEROS           TO GFCTNQ-COD-SQL-ERRO
                   MOVE 926             TO GFCTNQ-COD-MSG-ERRO
                   MOVE '0926'          TO GFCTG2-COD-MSG
                   PERFORM 1400-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTNQ-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

           PERFORM 2410-OBTER-DESC-SEGMENTO.
           MOVE ISGMTO-GSTAO-TARIF      OF GFCTB0H5
                                        TO GFCTNQ-DESCR-AGPTO.
           MOVE CSGMTO-GSTAO-TARIF      OF GFCTB0C0
                                        TO GFCTNQ-SEGMENTO.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM DESCRICAO DE SEGMENTO.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2410-OBTER-DESC-SEGMENTO        SECTION.
      *----------------------------------------------------------------*

           MOVE CSGMTO-GSTAO-TARIF  OF GFCTB0C0 TO
                                       CSGMTO-GSTAO-TARIF OF GFCTB0H5.

           EXEC SQL
             SELECT
                   ISGMTO_GSTAO_TARIF
             INTO
                   :GFCTB0H5.ISGMTO-GSTAO-TARIF
             FROM   DB2PRD.TTPO_SGMTO_GSTAO
             WHERE
                   CSGMTO_GSTAO_TARIF = :GFCTB0H5.CSGMTO-GSTAO-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES              TO GFCT0M-ERRO-SQL
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE 'GFCT0888'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'TTPO_SGMTO_GSTAO ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '          TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE             TO GFCT0M-SQLCODE
HEXA                                       WRK-COD-SQL-ERRO
.              MOVE WRK-COD-SQL-ERRO-2  TO GFCTNQ-COD-SQL-ERRO
               MOVE '0070'              TO GFCT0M-LOCAL
               MOVE SQLCA               TO GFCT0M-SQLCA-AREA
               MOVE 9                   TO GFCTNQ-ERRO
               MOVE 10                  TO GFCTNQ-COD-MSG-ERRO
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG     TO GFCTNQ-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           ELSE
               IF SQLCODE              EQUAL +100
                   MOVE 1               TO GFCTNQ-ERRO
                   MOVE ZEROS           TO GFCTNQ-COD-SQL-ERRO
                   MOVE 500             TO GFCTNQ-COD-MSG-ERRO
                   MOVE '0500'          TO GFCTG2-COD-MSG
                   PERFORM 1400-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTNQ-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    SELECIONA AGENCIA.                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-SELECIONAR-AGENCIA         SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF          OF GFCTB0B2
                                      TO CSERVC-TARIF       OF GFCTB0B1.
           MOVE HIDTFD-FLEXZ-AGPTO    OF GFCTB0B2
                                      TO HIDTFD-FLEXZ-AGPTO OF GFCTB0B1.
           MOVE CAGPTO-CTA            OF GFCTB0B2
                                      TO CAGPTO-CTA         OF GFCTB0B1.

           EXEC SQL
             SELECT
                   CEMPR_INC,
                   CDEPDC
             INTO
                   :GFCTB0B1.CEMPR-INC,
                   :GFCTB0B1.CDEPDC
             FROM   DB2PRD.PDIDO_FLEXZ_AG
             WHERE
                   CAGPTO_CTA         = :GFCTB0B1.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0B1.CSERVC-TARIF       AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0B1.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES              TO GFCT0M-ERRO-SQL
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE 'GFCT0888'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AG   ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '          TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE             TO GFCT0M-SQLCODE
HEXA                                       WRK-COD-SQL-ERRO
.              MOVE WRK-COD-SQL-ERRO-2  TO GFCTNQ-COD-SQL-ERRO
               MOVE '0080'              TO GFCT0M-LOCAL
               MOVE SQLCA               TO GFCT0M-SQLCA-AREA
               MOVE 9                   TO GFCTNQ-ERRO
               MOVE 10                  TO GFCTNQ-COD-MSG-ERRO
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG     TO GFCTNQ-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           ELSE
               IF SQLCODE              EQUAL +100
                   MOVE 1               TO GFCTNQ-ERRO
                   MOVE ZEROS           TO GFCTNQ-COD-SQL-ERRO
                   MOVE 926             TO GFCTNQ-COD-MSG-ERRO
                   MOVE '0926'          TO GFCTG2-COD-MSG
                   PERFORM 1400-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTNQ-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

           PERFORM 2510-OBTER-DESC-AGENCIA.
           MOVE GFCT1X-DESC-DEPDC(1)    TO GFCTNQ-DESCR-AGPTO.
           MOVE CDEPDC OF GFCTB0B1      TO GFCTNQ-AGENCIA.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM DESCRICAO DE AGENCIA.                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2510-OBTER-DESC-AGENCIA         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                         TO GFCT1V-ENTRADA.

           MOVE 660                            TO GFCT1V-LL.
           MOVE ZEROS                          TO GFCT1V-ZZ
                                                  GFCT1V-QTDE-TOT-REG.
           MOVE 1                              TO GFCT1V-QTDE-OCOR
           MOVE GFCTNP-TRANSACAO               TO GFCT1V-TRANSACAO.
           MOVE GFCTNP-FUNCAO                  TO GFCT1V-FUNCAO.
           MOVE GFCTNP-FUNC-BDSCO              TO GFCT1V-FUNC-BDSCO.
           MOVE 'N'                            TO GFCT1V-FIM.
           MOVE CDEPDC       OF GFCTB0B1       TO GFCT1V-COD-DEPDC(1).

           MOVE SPACES                         TO GFCT1X-SAIDA
                                                  GFCT0M-AREA-ERROS.
           INITIALIZE                          GFCT1X-SAIDA
                                               GFCT0M-AREA-ERROS.

           MOVE 'GFCT5006'                     TO WRK-MODULO.
           CALL 'POOL0081'                     USING WRK-MODULO
                                                     GFCT1V-ENTRADA
                                                     GFCT1X-SAIDA
                                                     GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                          TO GFCTNQ-ERRO
               MOVE WRK-MODULO                 TO GFCT0M-PROGRAMA
                                                  WRK-NOME-MODULO
               MOVE WRK-MSG05                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0888'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF GFCT1X-ERRO              NOT EQUAL ZEROS
               MOVE GFCT1X-ERRO                TO GFCTNQ-ERRO
               MOVE GFCT1X-COD-SQL-ERRO        TO GFCTNQ-COD-SQL-ERRO
               MOVE GFCT1X-COD-MSG-ERRO        TO GFCTNQ-COD-MSG-ERRO
               IF GFCT1X-ERRO        EQUAL 1
                   MOVE GFCT1X-DESC-MSG-ERRO   TO GFCTNQ-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5006-'   GFCT1X-DESC-MSG-ERRO
                   DELIMITED BY SIZE    INTO GFCTNQ-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    SELECIONA PAB.                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2600-SELECIONAR-PAB             SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF          OF GFCTB0B2
                                      TO CSERVC-TARIF       OF GFCTB0B8.
           MOVE HIDTFD-FLEXZ-AGPTO    OF GFCTB0B2
                                      TO HIDTFD-FLEXZ-AGPTO OF GFCTB0B8.
           MOVE CAGPTO-CTA            OF GFCTB0B2
                                      TO CAGPTO-CTA         OF GFCTB0B8.

           EXEC SQL
             SELECT
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
             INTO
                   :GFCTB0B8.CEMPR-INC,
                   :GFCTB0B8.CDEPDC,
                   :GFCTB0B8.CPOSTO-SERVC
             FROM   DB2PRD.PDIDO_FLEXZ_PAB
             WHERE
                   CAGPTO_CTA         = :GFCTB0B8.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0B8.CSERVC-TARIF       AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0B8.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES              TO GFCT0M-ERRO-SQL
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE 'GFCT0888'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_PAB  ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '          TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE             TO GFCT0M-SQLCODE
HEXA                                       WRK-COD-SQL-ERRO
.              MOVE WRK-COD-SQL-ERRO-2  TO GFCTNQ-COD-SQL-ERRO
               MOVE '0090'              TO GFCT0M-LOCAL
               MOVE SQLCA               TO GFCT0M-SQLCA-AREA
               MOVE 9                   TO GFCTNQ-ERRO
               MOVE 10                  TO GFCTNQ-COD-MSG-ERRO
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG     TO GFCTNQ-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           ELSE
               IF SQLCODE              EQUAL +100
                   MOVE 1               TO GFCTNQ-ERRO
                   MOVE ZEROS           TO GFCTNQ-COD-SQL-ERRO
                   MOVE 926             TO GFCTNQ-COD-MSG-ERRO
                   MOVE '0926'          TO GFCTG2-COD-MSG
                   PERFORM 1400-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTNQ-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

           PERFORM 2610-OBTER-DESC-PAB.
           MOVE GFCTGC-DESC-POSTO(1)        TO GFCTNQ-DESCR-AGPTO.
           MOVE CDEPDC       OF GFCTB0B8    TO GFCTNQ-AGENCIA.
           MOVE CPOSTO-SERVC OF GFCTB0B8    TO GFCTNQ-POSTO.

      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM DESCRICAO DE PAB.                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2610-OBTER-DESC-PAB             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                         TO GFCTGB-ENTRADA.

           MOVE 100                            TO GFCTGB-LL.
           MOVE ZEROS                          TO GFCTGB-ZZ
                                                  GFCTGB-QTDE-OCOR
                                                  GFCTGB-QTDE-TOT-REG.
           MOVE GFCTNP-TRANSACAO               TO GFCTGB-TRANSACAO.
           MOVE GFCTNP-FUNCAO                  TO GFCTGB-FUNCAO.
           MOVE GFCTNP-FUNC-BDSCO              TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                            TO GFCTGB-FIM.
           MOVE CDEPDC       OF GFCTB0B8       TO GFCTGB-COD-DEPDC.
           MOVE CPOSTO-SERVC OF GFCTB0B8       TO GFCTGB-COD-POSTO.

           MOVE SPACES                         TO GFCTGC-SAIDA
                                                  GFCT0M-AREA-ERROS.
           INITIALIZE                          GFCTGC-SAIDA
                                               GFCT0M-AREA-ERROS.

           MOVE 'GFCT5109'                     TO WRK-MODULO.
           CALL 'POOL0081'                     USING WRK-MODULO
                                                     GFCTGB-ENTRADA
                                                     GFCTGC-SAIDA
                                                     GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                          TO GFCTNQ-ERRO
               MOVE WRK-MODULO                 TO GFCT0M-PROGRAMA
                                                  WRK-NOME-MODULO
               MOVE WRK-MSG05                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0888'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF GFCTGC-ERRO              NOT EQUAL ZEROS
               MOVE 'S'                        TO GFCTNQ-FIM
               MOVE GFCTGC-ERRO                TO GFCTNQ-ERRO
               MOVE GFCTGC-COD-SQL-ERRO        TO GFCTNQ-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO        TO GFCTNQ-COD-MSG-ERRO
               IF GFCTGC-ERRO        EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO   TO GFCTNQ-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5109-'   GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE    INTO GFCTNQ-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    SELECIONA POSTAL.                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2700-SELECIONAR-POSTAL          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF          OF GFCTB0B2
                                      TO CSERVC-TARIF       OF GFCTB0B9.
           MOVE HIDTFD-FLEXZ-AGPTO    OF GFCTB0B2
                                      TO HIDTFD-FLEXZ-AGPTO OF GFCTB0B9.
           MOVE CAGPTO-CTA            OF GFCTB0B2
                                      TO CAGPTO-CTA         OF GFCTB0B9.

           EXEC SQL
             SELECT
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
             INTO
                   :GFCTB0B9.CEMPR-INC,
                   :GFCTB0B9.CDEPDC,
                   :GFCTB0B9.CPOSTO-SERVC
             FROM   DB2PRD.PDIDO_FLEXZ_PSTAL
             WHERE
                   CAGPTO_CTA         = :GFCTB0B9.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0B9.CSERVC-TARIF       AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0B9.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES                     TO GFCT0M-ERRO-SQL
               INITIALIZE                      GFCT0M-ERRO-SQL
               MOVE 'GFCT0888'                 TO GFCT0M-PROGRAMA
               MOVE 'DB2'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_PSTAL'        TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '                 TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE                    TO GFCT0M-SQLCODE
HEXA                                              WRK-COD-SQL-ERRO
.              MOVE WRK-COD-SQL-ERRO-2         TO GFCTNQ-COD-SQL-ERRO
               MOVE '0100'                     TO GFCT0M-LOCAL
               MOVE SQLCA                      TO GFCT0M-SQLCA-AREA
               MOVE 9                          TO GFCTNQ-ERRO
               MOVE 10                         TO GFCTNQ-COD-MSG-ERRO
               MOVE '0010'                     TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG            TO GFCTNQ-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           ELSE
               IF SQLCODE                      EQUAL +100
                   MOVE 1                      TO GFCTNQ-ERRO
                   MOVE ZEROS                  TO GFCTNQ-COD-SQL-ERRO
                   MOVE 926                    TO GFCTNQ-COD-MSG-ERRO
                   MOVE '0926'                 TO GFCTG2-COD-MSG
                   PERFORM 1400-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG        TO GFCTNQ-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

           MOVE SPACES                         TO GFCT0M-ERRO-SQL
           MOVE 100                            TO GFCTGB-LL.
           MOVE ZEROS                          TO GFCTGB-ZZ
                                                  GFCTGB-QTDE-OCOR
                                                  GFCTGB-QTDE-TOT-REG.
           MOVE GFCTNP-TRANSACAO               TO GFCTGB-TRANSACAO.
           MOVE GFCTNP-FUNCAO                  TO GFCTGB-FUNCAO.
           MOVE GFCTNP-FUNC-BDSCO              TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                            TO GFCTGB-FIM.
           MOVE CDEPDC       OF GFCTB0B9       TO GFCTGB-COD-DEPDC.
           MOVE CPOSTO-SERVC OF GFCTB0B9       TO GFCTGB-COD-POSTO.

           MOVE SPACES                         TO GFCTGC-SAIDA
                                                  GFCT0M-AREA-ERROS.
           INITIALIZE                          GFCTGC-SAIDA
                                               GFCT0M-AREA-ERROS.

           MOVE 'GFCT5090'                     TO WRK-MODULO.
           CALL 'POOL0081'                     USING WRK-MODULO
                                                     GFCTGB-ENTRADA
                                                     GFCTGC-SAIDA
                                                     GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                     NOT EQUAL ZEROS
               MOVE 9                          TO GFCTNQ-ERRO
               MOVE WRK-MODULO                 TO GFCT0M-PROGRAMA
                                                  WRK-NOME-MODULO
               MOVE WRK-MSG05                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0888'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF GFCTGC-ERRO                      NOT EQUAL ZEROS
               MOVE GFCTGC-ERRO                TO GFCTNQ-ERRO
               MOVE GFCTGC-COD-SQL-ERRO        TO GFCTNQ-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO        TO GFCTNQ-COD-MSG-ERRO
               IF GFCTG3-ERRO        EQUAL 1
                   MOVE GFCTG3-DESC-MSG-ERRO   TO GFCTNQ-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5090-'   GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE    INTO GFCTNQ-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE GFCTGC-DESC-POSTO(1)           TO GFCTNQ-DESCR-AGPTO.
           MOVE CDEPDC       OF GFCTB0B9       TO GFCTNQ-AGENCIA.
           MOVE CPOSTO-SERVC OF GFCTB0B9       TO GFCTNQ-POSTO.

      *----------------------------------------------------------------*
       2700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

LFM   ******************************************************************
LFM   *    SELECIONA RAZAO.                                            *
LFM   ******************************************************************
LFM   *----------------------------------------------------------------*
LFM    2710-SELECIONAR-RAZAO           SECTION.
LFM   *----------------------------------------------------------------*

           MOVE CSERVC-TARIF          OF GFCTB0B2
                                      TO CSERVC-TARIF       OF GFCTB0M7.
           MOVE HIDTFD-FLEXZ-AGPTO    OF GFCTB0B2
                                      TO HIDTFD-FLEXZ-AGPTO OF GFCTB0M7.
           MOVE CAGPTO-CTA            OF GFCTB0B2
                                      TO CAGPTO-CTA         OF GFCTB0M7.
LFM
LFM        EXEC SQL
LFM          SELECT
0710XS             CGRP_CTBIL_ATUAL,
0710XS             CSGRP_CTBIL
LFM          INTO
0710XS             :GFCTB0M7.CGRP-CTBIL-ATUAL,
0710XS             :GFCTB0M7.CSGRP-CTBIL
LFM          FROM   DB2PRD.TPDIDO_FLEXZ_RZ
LFM          WHERE
                   CAGPTO_CTA         = :GFCTB0M7.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0M7.CSERVC-TARIF       AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0M7.HIDTFD-FLEXZ-AGPTO
LFM        END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES                     TO GFCT0M-ERRO-SQL
               INITIALIZE                      GFCT0M-ERRO-SQL
               MOVE 'GFCT0888'                 TO GFCT0M-PROGRAMA
               MOVE 'DB2'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'TPDIDO_FLEXZ_RZ'          TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '                 TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE                    TO GFCT0M-SQLCODE
HEXA                                              WRK-COD-SQL-ERRO
.              MOVE WRK-COD-SQL-ERRO-2         TO GFCTNQ-COD-SQL-ERRO
               MOVE '0100'                     TO GFCT0M-LOCAL
               MOVE SQLCA                      TO GFCT0M-SQLCA-AREA
               MOVE 9                          TO GFCTNQ-ERRO
               MOVE 10                         TO GFCTNQ-COD-MSG-ERRO
               MOVE '0010'                     TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG            TO GFCTNQ-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           ELSE
               IF SQLCODE                      EQUAL +100
                   MOVE 1                      TO GFCTNQ-ERRO
                   MOVE ZEROS                  TO GFCTNQ-COD-SQL-ERRO
                   MOVE 926                    TO GFCTNQ-COD-MSG-ERRO
                   MOVE '0926'                 TO GFCTG2-COD-MSG
                   PERFORM 1400-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG        TO GFCTNQ-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

           MOVE SPACES                         TO GFCT0M-ERRO-SQL
           MOVE 100                            TO GFCTGB-LL.
           MOVE ZEROS                          TO GFCTGB-ZZ
                                                  GFCTGB-QTDE-OCOR
                                                  GFCTGB-QTDE-TOT-REG.
           MOVE GFCTNP-TRANSACAO               TO GFCTGB-TRANSACAO.
           MOVE GFCTNP-FUNCAO                  TO GFCTGB-FUNCAO.
           MOVE GFCTNP-FUNC-BDSCO              TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                            TO GFCTGB-FIM.
           MOVE CDEPDC       OF GFCTB0B9       TO GFCTGB-COD-DEPDC.
           MOVE CPOSTO-SERVC OF GFCTB0B9       TO GFCTGB-COD-POSTO.

LFM        MOVE GFCTGC-DESC-POSTO(1)           TO GFCTNQ-DESCR-AGPTO.
LFM        MOVE CDEPDC       OF GFCTB0B9       TO GFCTNQ-AGENCIA.
LFM        MOVE CPOSTO-SERVC OF GFCTB0B9       TO GFCTNQ-POSTO.

0710XS     MOVE CGRP-CTBIL-ATUAL OF GFCTB0M7
0710XS                                 TO GFCTNQ-GRUPO-CTBIL.
0710XS     MOVE CSGRP-CTBIL OF GFCTB0M7
0710XS                                 TO GFCTNQ-SUB-GRUPO-CTBIL.

LFM   *----------------------------------------------------------------*
LFM    2710-99-FIM.                    EXIT.
LFM   *----------------------------------------------------------------*

      ******************************************************************
      *    SELECIONA MUNICIPIO.                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2800-SELECIONAR-MUNICIPIO       SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF          OF GFCTB0B2
                                      TO CSERVC-TARIF       OF GFCTB0B7.
           MOVE HIDTFD-FLEXZ-AGPTO    OF GFCTB0B2
                                      TO HIDTFD-FLEXZ-AGPTO OF GFCTB0B7.
           MOVE CAGPTO-CTA            OF GFCTB0B2
                                      TO CAGPTO-CTA         OF GFCTB0B7.

           EXEC SQL
             SELECT
                   CMUN_IBGE
             INTO
                   :GFCTB0B7.CMUN-IBGE
             FROM   DB2PRD.PDIDO_FLEXZ_MUN
             WHERE
                   CAGPTO_CTA         = :GFCTB0B7.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0B7.CSERVC-TARIF       AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0B7.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES                     TO GFCT0M-ERRO-SQL
               INITIALIZE                      GFCT0M-ERRO-SQL
               MOVE 'GFCT0888'                 TO GFCT0M-PROGRAMA
               MOVE 'DB2'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_MUN  '        TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '                 TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE                    TO GFCT0M-SQLCODE
HEXA                                              WRK-COD-SQL-ERRO
.              MOVE WRK-COD-SQL-ERRO-2         TO GFCTNQ-COD-SQL-ERRO
               MOVE '0110'                     TO GFCT0M-LOCAL
               MOVE SQLCA                      TO GFCT0M-SQLCA-AREA
               MOVE 9                          TO GFCTNQ-ERRO
               MOVE 10                         TO GFCTNQ-COD-MSG-ERRO
               MOVE '0010'                     TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG            TO GFCTNQ-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           ELSE
               IF SQLCODE              EQUAL +100
                   MOVE 1                      TO GFCTNQ-ERRO
                   MOVE ZEROS                  TO GFCTNQ-COD-SQL-ERRO
                   MOVE 926                    TO GFCTNQ-COD-MSG-ERRO
                   MOVE '0926'                 TO GFCTG2-COD-MSG
                   PERFORM 1400-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG        TO GFCTNQ-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

           MOVE SPACES                         TO AREA-RURC88.
           INITIALIZE                          AREA-RURC88.

           MOVE CMUN-IBGE OF GFCTB0B7          TO COD-MUNIC-RURC88.

           MOVE 'RURC9020'                     TO WRK-MODULO.
           CALL 'POOL0081'                     USING WRK-MODULO
                                                     AREA-RURC88.

           IF  RETURN-CODE             NOT EQUAL ZEROS
                MOVE 1                         TO GFCTNQ-ERRO
                MOVE ZEROS                     TO GFCTNQ-COD-SQL-ERRO
                MOVE 732                       TO GFCTNQ-COD-MSG-ERRO
                MOVE '0732'                    TO GFCTG2-COD-MSG
                PERFORM 1400-OBTER-DESC-MENSAGEM
                MOVE GFCTG3-DESC-MSG           TO GFCTNQ-DESC-MSG-ERRO
                PERFORM 3000-FINALIZAR
           END-IF.

           EVALUATE COD-RETORNO-RURC88
               WHEN 10
                   MOVE 1                      TO GFCTNQ-ERRO
                   MOVE ZEROS                  TO GFCTNQ-COD-SQL-ERRO
                   MOVE 675                    TO GFCTNQ-COD-MSG-ERRO
                   MOVE '0675'                 TO GFCTG2-COD-MSG
                   PERFORM 1400-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG        TO GFCTNQ-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               WHEN 20
                   MOVE 1                      TO GFCTNQ-ERRO
                   MOVE ZEROS                  TO GFCTNQ-COD-SQL-ERRO
                   MOVE 733                    TO GFCTNQ-COD-MSG-ERRO
                   MOVE '0733'                 TO GFCTG2-COD-MSG
                   PERFORM 1400-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG        TO GFCTNQ-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               WHEN 99
                   MOVE 1                      TO GFCTNQ-ERRO
                   MOVE SQLCODE-RURC88         TO GFCTNQ-COD-SQL-ERRO
                   MOVE 734                    TO GFCTNQ-COD-MSG-ERRO
                   MOVE '0734'                 TO GFCTG2-COD-MSG
                   PERFORM 1400-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG        TO GFCTNQ-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
           END-EVALUATE.

           MOVE NOME-MUNIC-RURC88              TO GFCTNQ-DESCR-AGPTO.
           MOVE CMUN-IBGE OF GFCTB0B7          TO GFCTNQ-MUNICIPIO.

      *----------------------------------------------------------------*
       2800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    SELECIONA UNIDADE FEDERATIVA.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2900-SELECIONAR-UF              SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF          OF GFCTB0B2
                                      TO CSERVC-TARIF       OF GFCTB0C1.
           MOVE HIDTFD-FLEXZ-AGPTO    OF GFCTB0B2
                                      TO HIDTFD-FLEXZ-AGPTO OF GFCTB0C1.
           MOVE CAGPTO-CTA            OF GFCTB0B2
                                      TO CAGPTO-CTA         OF GFCTB0C1.

           EXEC SQL
             SELECT
                   CSGL_UF
             INTO
                   :GFCTB0C1.CSGL-UF
             FROM   DB2PRD.PDIDO_FLEXZ_UF
             WHERE
                   CAGPTO_CTA         = :GFCTB0C1.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0C1.CSERVC-TARIF       AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0C1.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES                     TO GFCT0M-ERRO-SQL
               INITIALIZE                      GFCT0M-ERRO-SQL
               MOVE 'GFCT0888'                 TO GFCT0M-PROGRAMA
               MOVE 'DB2'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_UF   '        TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '                 TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE                    TO GFCT0M-SQLCODE
HEXA                                              WRK-COD-SQL-ERRO
.              MOVE WRK-COD-SQL-ERRO-2         TO GFCTNQ-COD-SQL-ERRO
               MOVE '0120'                     TO GFCT0M-LOCAL
               MOVE SQLCA                      TO GFCT0M-SQLCA-AREA
               MOVE 9                          TO GFCTNQ-ERRO
               MOVE 10                         TO GFCTNQ-COD-MSG-ERRO
               MOVE '0010'                     TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG            TO GFCTNQ-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           ELSE
               IF SQLCODE              EQUAL +100
                   MOVE 1                      TO GFCTNQ-ERRO
                   MOVE ZEROS                  TO GFCTNQ-COD-SQL-ERRO
                   MOVE 926                    TO GFCTNQ-COD-MSG-ERRO
                   MOVE '0926'                 TO GFCTG2-COD-MSG
                   PERFORM 1400-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG        TO GFCTNQ-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

           MOVE CSGL-UF   OF GFCTB0C1  TO CSGL-UF OF DCITV002.
           MOVE DCITV002               TO WRK-AREA-DCLGEN-DCITV002.


           MOVE ZEROS                          TO WRK-DCIT-COD-RETORNO.

           MOVE 'DCIT8000'                     TO WRK-MODULO.
           CALL 'POOL0081'            USING WRK-MODULO
                                            WRK-DCIT-UNIDADE-FEDERATIVA
                                            WRK-POOL7100
                                            WRK-SQLCA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                          TO GFCTNQ-ERRO
               MOVE ZEROS                      TO GFCTNQ-COD-SQL-ERRO
               MOVE 735                        TO GFCTNQ-COD-MSG-ERRO
               MOVE '0735'                     TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG            TO GFCTNQ-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           EVALUATE WRK-DCIT-COD-RETORNO
               WHEN 1
                   MOVE 1                      TO GFCTNQ-ERRO
                   MOVE ZEROS                  TO GFCTNQ-COD-SQL-ERRO
                   MOVE 736                    TO GFCTNQ-COD-MSG-ERRO
                   MOVE '0736'                 TO GFCTG2-COD-MSG
                   PERFORM 1400-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG        TO GFCTNQ-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               WHEN 2
                   MOVE 1                      TO GFCTNQ-ERRO
                   MOVE ZEROS                  TO GFCTNQ-COD-SQL-ERRO
                   MOVE 737                    TO GFCTNQ-COD-MSG-ERRO
                   MOVE '0737'                 TO GFCTG2-COD-MSG
                   PERFORM 1400-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG        TO GFCTNQ-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               WHEN 3
                   MOVE 1                      TO GFCTNQ-ERRO
                   MOVE ZEROS                  TO GFCTNQ-COD-SQL-ERRO
                   MOVE 738                    TO GFCTNQ-COD-MSG-ERRO
                   MOVE '0738'                 TO GFCTG2-COD-MSG
                   PERFORM 1400-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG        TO GFCTNQ-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               WHEN 4
                   MOVE SPACES                 TO GFCT0M-ERRO-SQL
                   INITIALIZE GFCT0M-ERRO-SQL
                   MOVE 'DCITV002'             TO WRK-NOME-TAB
                   MOVE 'V01UF'                TO GFCT0M-NOME-TAB
                   MOVE 'SELECT'               TO GFCT0M-COMANDO-SQL
                   MOVE '0047'                 TO GFCT0M-LOCAL
                   MOVE 5                      TO GFCTNQ-COD-MSG-ERRO
                   MOVE '0005'                 TO GFCTG2-COD-MSG
                   MOVE 9                      TO GFCTNQ-ERRO
                   MOVE 'GFCT0888'             TO GFCT0M-PROGRAMA
                   MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO
                   MOVE WRK-SQLCA              TO SQLCA
                                                  GFCT0M-SQLCA-AREA
                   MOVE SQLCODE                TO GFCT0M-SQLCODE
HEXA                                              WRK-COD-SQL-ERRO
.                  MOVE WRK-COD-SQL-ERRO-2     TO GFCTNQ-COD-SQL-ERRO
                   MOVE SPACES                 TO GFCTNQ-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
           END-EVALUATE.

           MOVE WRK-AREA-DCLGEN-DCITV002       TO DCITV002.
           MOVE IUF OF DCITV002                TO GFCTNQ-DESCR-AGPTO.
           MOVE CSGL-UF OF GFCTB0C1            TO GFCTNQ-UF.

      *----------------------------------------------------------------*
       2900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

110608******************************************************************
110608*    PESQUISA NOME DOS AGRUPAMENTOS PERSONALIZADOS.              *
110608******************************************************************
110608*----------------------------------------------------------------*
110608 2910-PESQUISA-AGRUPAMENTO       SECTION.
110608*----------------------------------------------------------------*
110608
110608     MOVE CAGPTO-CTA            OF GFCTB0B2
110608                                TO CAGPTO-CTA OF GFCTB0F3.
110608
110608     EXEC SQL
110608       SELECT  RAGPTO_CTA
110608         INTO  :GFCTB0F3.RAGPTO-CTA
110608         FROM  DB2PRD.TPO_AGPTO_CTA
110608        WHERE  CAGPTO_CTA         = :GFCTB0F3.CAGPTO-CTA
110608     END-EXEC.
110608
110608     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
110608        (SQLWARN0                EQUAL 'W'      )
110608         MOVE SPACES                     TO GFCT0M-ERRO-SQL
110608         INITIALIZE                      GFCT0M-ERRO-SQL
110608         MOVE 'GFCT0888'                 TO GFCT0M-PROGRAMA
110608         MOVE 'DB2'                      TO GFCT0M-TIPO-ACESSO
110608         MOVE 'TPO_AGPTO_CTA    '        TO GFCT0M-NOME-TAB
110608         MOVE 'SELECT  '                 TO GFCT0M-COMANDO-SQL
110608         MOVE SQLCODE                    TO GFCT0M-SQLCODE
HEXA                                              WRK-COD-SQL-ERRO
.              MOVE WRK-COD-SQL-ERRO-2         TO GFCTNQ-COD-SQL-ERRO
110608         MOVE '0130'                     TO GFCT0M-LOCAL
110608         MOVE SQLCA                      TO GFCT0M-SQLCA-AREA
110608         MOVE 9                          TO GFCTNQ-ERRO
110608         MOVE 10                         TO GFCTNQ-COD-MSG-ERRO
110608         MOVE '0010'                     TO GFCTG2-COD-MSG
110608         PERFORM 1400-OBTER-DESC-MENSAGEM
110608         MOVE GFCTG3-DESC-MSG            TO GFCTNQ-DESC-MSG-ERRO
110608         PERFORM 3000-FINALIZAR
110608     ELSE
110608         IF SQLCODE              EQUAL +100
110608             MOVE 1                      TO GFCTNQ-ERRO
110608             MOVE ZEROS                  TO GFCTNQ-COD-SQL-ERRO
110608             MOVE 923                    TO GFCTNQ-COD-MSG-ERRO
110608             MOVE '0923'                 TO GFCTG2-COD-MSG
110608             PERFORM 1400-OBTER-DESC-MENSAGEM
110608             MOVE GFCTG3-DESC-MSG        TO GFCTNQ-DESC-MSG-ERRO
110608             PERFORM 3000-FINALIZAR
110608         END-IF
110608     END-IF.
110608
110608     MOVE RAGPTO-CTA OF GFCTB0F3         TO GFCTNQ-DESCR-AGPTO.
110608
110608*----------------------------------------------------------------*
110608 2910-99-FIM.                    EXIT.
110608*----------------------------------------------------------------*

      ******************************************************************
      *    FINALIZA O PROCESSAMENTO E RETORNA AO PROGRAMA CHAMADOR.    *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
