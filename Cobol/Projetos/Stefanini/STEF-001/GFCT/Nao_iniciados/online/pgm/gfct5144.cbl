      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5144.
       AUTHOR.     JORGE VARGAS.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT5144                                    *
      *    PROGRAMADOR.:   JORGE VARGAS            - CPM PATO BRANCO   *
      *    ANALISTA CPM:   ADRIANO LOPES SANTANNA  - CPM PATO BRANCO   *
      *    ANALISTA....:   MARCELO CREMM           - PROCWORK /GP.50   *
      *    DATA........:   13/04/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   LISTA DE  ARGUMENTOS DA EXCECAO. LISTAR     *
      *      ATE 8 OCORRENCIAS NA SAIDA E MONTAR RESTART, CASO HAJA,   *
      *      COM A  CHAVE  DA PROXIMA OCORRENCIA  VALIDA  DO CURSOR    *
      *      (PROXIMA SEQUENCIA SE HOUVER).                            *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                        INCLUDE/BOOK     *
      *                   DB2PRD.PARM_DATA_PROCM        GFCTB0A1       *
      *                   DB2PRD.EXEC_ADSAO_COMP        GFCTB048       *
      *                   DB2PRD.ADSAO_GRP_MUN          GFCTB004       *
      *                   DB2PRD.ADSAO_GRP_PAB          GFCTB005       *
      *                   DB2PRD.ADSAO_GRP_PSTAl        GFCTB006       *
      *                   DB2PRD.ADSAO_GRP_SGMTO        GFCTB007       *
      *                   DB2PRD.ADSAO_GRP_UF           GFCTB008       *
      *                   DB2PRD.TADSAO_GRP_CLI         GFCTB0H9       *
      *                   DB2PRD.TTPO_SGMTO_GSTAO       GFCTB0H5       *
      *                   DB2PRD.V01UF                  DCITV002       *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#GFCT0H - AREA DE COMUNICACAO - ENTRADA.                   *
      *    I#GFCT0I - AREA DE COMUNICACAO - SAIDA.                     *
      *    I#GFCTFZ - AREA DE COMUNICACAO - ENTRADA.                   *
      *    I#GFCTG1 - AREA DE COMUNICACAO - SAIDA.                     *
      *    I#GFCTG2 - AREA DE COMUNICACAO - ENTRADA.                   *
      *    I#GFCTG3 - AREA DE COMUNICACAO - SAIDA.                     *
      *    I#GFCTGB - AREA DE COMUNICACAO - ENTRADA.                   *
      *    I#GFCTGC - AREA DE COMUNICACAO - SAIDA.                     *
      *    I#GFCT1V - AREA DE COMUNICACAO - ENTRADA.                   *
      *    I#GFCT1X - AREA DE COMUNICACAO - SAIDA.                     *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *    I#RURC88 - AREA PARA MODULO RURC9020.                       *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                  *
      *    GFCT5109 - OBTER DESCRICAO DE PAB.                          *
      *    GFCT5522 - VERIFICAR ONLINE.                                *
      *    GFCT5006 - DESCRICAO DEPENDENCIA.                           *
      *    GFCT5090 - DESCRICAO POSTO SERVICO.                         *
      *    DCIT8000 - OBTEM UF.                                        *
      *    RURC9020 - OBTEM MUNICIPIO.                                 *
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

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA INDEXADORES *'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(001)         VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.

       77  WRK-NOME-TAB                PIC  X(008)         VALUE SPACES.

       77  WRK-CSR01-FIM-GFCTB048      PIC  X(001)         VALUE SPACES.

       01  WRK-DPROCM.
           05 WRK-DPROCM-ANO           PIC  9(004)         VALUE ZEROS.
           05 WRK-DPROCM-MES           PIC  9(002)         VALUE ZEROS.
           05 WRK-DPROCM-DIA           PIC  9(002)         VALUE ZEROS.

       01  FILLER.
           05  WRK-SEQUENCIA           PIC  9(009)         VALUE ZEROS.

       01  WRK-MSG005.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA AO MODULO '.
           05 WRK-NOME-MODULO          PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO DO MODULO. DCIT8000 *'.
      *----------------------------------------------------------------*

       01  WRK-DCIT-UNIDADE-FEDERATIVA.
           05 WRK-AREA-DCLGEN-DCITV002 PIC  X(070)         VALUE SPACES.
           05 WRK-DCIT-CODIGO-RETORNO  PIC  9(002)         VALUE ZEROS.

       01  WRK-POOL7100                PIC  X(107)         VALUE SPACES.

       01  WRK-SQLCA                   PIC  X(136)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA DE INDICADORES DE NULO *'.
      *---------------------------------------------------------------*

       01  WRK-GFCTB007-NULL.
           05 WRK-CSGMTO-GSTAO-TARIF-NULL
                                       PIC S9(04) COMP     VALUE +0.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5522 - ENTRADA *'.
      *---------------------------------------------------------------*

       COPY I#GFCTFZ.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5522 - SAIDA *'.
      *---------------------------------------------------------------*

       COPY I#GFCTG1.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5523 - ENTRADA *'.
      *---------------------------------------------------------------*

       COPY I#GFCTG2.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5523 - SAIDA *'.
      *---------------------------------------------------------------*

       COPY I#GFCTG3.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE PARA GFCT5109 - ENTRADA *'.
      *---------------------------------------------------------------*

       COPY I#GFCTGB.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE PARA GFCT5109 - SAIDA *'.
      *---------------------------------------------------------------*

       COPY I#GFCTGC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE PARA GFCT5209 - ENTRADA *'.
      *---------------------------------------------------------------*

       COPY GFCTWABC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE PARA GFCT5209 - SAIDA *'.
      *---------------------------------------------------------------*

       COPY GFCTWABD.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO GFCT5006 - ENTRADA *'.
      *---------------------------------------------------------------*

       COPY I#GFCT1V.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO GFCT5006 - SAIDA *'.
      *---------------------------------------------------------------*

       COPY I#GFCT1X.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO RURC9020 *'.
      *---------------------------------------------------------------*


       COPY I#RURC88.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TABELA DB2 *'.
      *---------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB048
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB002
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB004
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB005
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB006
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB007
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB008
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0H9
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0H5
           END-EXEC.

           EXEC SQL
             INCLUDE DCITV002
           END-EXEC.

           EXEC SQL
             DECLARE   CSR01-GFCTB048  CURSOR   FOR
               SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_ADSAO_COMP,
                   CJUNC_DEPDC,
                   CCTA_CLI,
                   HINCL_EXCEC_ADSAO,
                   CFUNC_INCL_EXCEC,
                   CFUNC_EXCL_EXCEC,
                   HEXCL_EXCEC_ADSAO,
                   CINDCD_EXCL_REG,
                   DINIC_EXCEC_ADSAO,
                   DFIM_EXCEC_ADSAO
             FROM  DB2PRD.EXCEC_ADSAO_COMP
             WHERE
                   CSERVC_TARIF       =  :GFCTB048.CSERVC-TARIF   AND
                   CAGPTO_CTA         =  :GFCTB048.CAGPTO-CTA     AND
                   CSEQ_AGPTO_CTA     >= :GFCTB048.CSEQ-AGPTO-CTA
                   ORDER BY CSEQ_AGPTO_CTA
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY I#GFCTOH.

       COPY I#GFCTOI.

       COPY I#GFCT0M.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTOH-ENTRADA
                                                      GFCTOI-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *CHAMA AS ROTINAS PRINCIPAIS DO PROCESSAMENTO                    *
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
      *PROCEDIMENTOS INICIAIS DO PROGRAMA                              *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          GFCTOI-SAIDA.

           INITIALIZE GFCTOI-SAIDA
                      GFCT0M-AREA-ERROS.

           PERFORM 1100-VERIFICAR-ON-LINE.

           PERFORM 1200-MONTAR-AREA-DE-SAIDA.

           PERFORM 1300-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *SITUACAO DO SISTEMA ( LIBERADO OU BLOQUEADO )                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-VERIFICAR-ON-LINE          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTOH-TRANSACAO       TO GFCTFZ-TRANSACAO.
           MOVE GFCTOH-FUNCAO          TO GFCTFZ-FUNCAO.
           MOVE GFCTOH-FUNC-BDSCO      TO GFCTFZ-FUNC-BDSCO.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE 'N'                    TO GFCTFZ-FIM.

           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                       WRK-NOME-MODULO
               MOVE WRK-MSG005         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5144'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTOI-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 9
               MOVE 'GFCT5144'         TO GFCT0M-TRANSACAO
               MOVE GFCTG1-COD-SQL-ERRO
                                       TO GFCTOI-COD-SQL-ERRO
               MOVE GFCTG1-ERRO        TO GFCTOI-ERRO
               MOVE GFCTG1-COD-MSG-ERRO
                                       TO GFCTOI-COD-MSG-ERRO
               MOVE GFCTG1-DESC-MSG-ERRO
                                       TO GFCTOI-DESC-MSG-ERRO
                                          GFCT0M-TEXTO
               PERFORM 3000-FINALIZAR
            END-IF.

            IF GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 'SISTEMA INDISPONIVEL'
                                       TO GFCT0M-TEXTO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5144'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTOI-ERRO
               PERFORM 3000-FINALIZAR
            END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * MONTA A AREA DE SAIDA                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-MONTAR-AREA-DE-SAIDA       SECTION.
      *----------------------------------------------------------------*

           MOVE 314                    TO GFCTOI-LL.
           MOVE ZEROS                  TO GFCTOI-ZZ.
           MOVE GFCTOH-TRANSACAO       TO GFCTOI-TRANSACAO
           MOVE GFCTOH-FUNCAO          TO GFCTOI-FUNCAO

           IF  GFCTOH-QTDE-OCOR        IS NUMERIC
               MOVE GFCTOH-QTDE-OCOR   TO GFCTOI-QTDE-OCOR
           ELSE
               MOVE ZEROS              TO GFCTOI-QTDE-OCOR
           END-IF.

           IF  GFCTOH-QTDE-TOT-REG     IS NUMERIC
               MOVE GFCTOH-QTDE-TOT-REG
                                       TO GFCTOI-QTDE-TOT-REG
           ELSE
               MOVE ZEROS              TO GFCTOI-QTDE-TOT-REG
           END-IF.

           MOVE GFCTOH-FUNC-BDSCO      TO GFCTOI-FUNC-BDSCO
           MOVE GFCTOH-FILTRO          TO GFCTOI-FILTRO.
           MOVE GFCTOH-PONTEIRO        TO GFCTOI-PONTEIRO.

           MOVE 'N'                    TO GFCTOI-FIM.

           MOVE ZEROS                  TO GFCTOI-ERRO
                                          GFCTOI-COD-SQL-ERRO
                                          GFCTOI-COD-MSG-ERRO.

           MOVE '0000'                 TO GFCTG2-COD-MSG.

           PERFORM 1210-OBTER-DESC-MSG.

           MOVE GFCTG3-DESC-MSG        TO GFCTOI-DESC-MSG-ERRO.

           MOVE ZEROS                  TO WRK-SEQUENCIA.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTEM DESCRICAO DA MENSAGEM                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1210-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE 100                    TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.

           MOVE GFCTOH-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTOH-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTOH-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'N'                    TO GFCTG2-FIM.

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE 'GFCT5523'             TO WRK-MODULO

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                       WRK-NOME-MODULO
               MOVE WRK-MSG005         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5144'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTOI-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 9                  TO GFCTOI-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                       WRK-NOME-MODULO
               MOVE 'S'                TO GFCTOI-FIM
               MOVE WRK-MSG005         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5144'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO               EQUAL 1
               MOVE 'S'                  TO GFCTOI-FIM
               MOVE 1                    TO GFCTOI-ERRO
               MOVE GFCTG3-COD-SQL-ERRO  TO GFCTOI-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO  TO GFCTOI-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO TO GFCTOI-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTE DADOS                                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (GFCTOH-TRANSACAO        EQUAL SPACES OR LOW-VALUES) OR
              (GFCTOH-FUNCAO           EQUAL SPACES OR LOW-VALUES) OR
              (GFCTOH-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES) OR
              (GFCTOH-FILTRO-PACOTE    NOT NUMERIC)                OR
              (GFCTOH-FILTRO-PACOTE    EQUAL ZEROS)                OR
              (GFCTOH-FILTRO-AGPTO     NOT NUMERIC)                OR
              (GFCTOH-FILTRO-AGPTO     NOT EQUAL 004 AND 009 AND
                                                 014 AND 015 AND
                                                 017 AND 018 AND
                                                 019 AND 020)      OR
              (GFCTOH-PONT-PACOTE      NOT NUMERIC)                OR
              (GFCTOH-PONT-AGPTO       NOT NUMERIC)                OR
              (GFCTOH-PONT-SEQ         NOT NUMERIC)                OR
              (GFCTOH-FIM              NOT EQUAL 'N' AND 'S')
               MOVE  'S'               TO GFCTOI-FIM
               MOVE  1                 TO GFCTOI-ERRO
               MOVE  0001              TO GFCTOI-COD-MSG-ERRO
               MOVE  '0001'            TO GFCTG2-COD-MSG
               MOVE  ZEROS             TO GFCTOI-COD-SQL-ERRO
               PERFORM 1210-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTOI-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 1310-SELECIONAR-GFCTB0A1.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FAZ A SELECAO EM GFCTB0A1.                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       1310-SELECIONAR-GFCTB0A1        SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ATUAL
             INTO
                   :GFCTB0A1.DPROCM-ATUAL
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                   CSIST_PRINC        = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTOI-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A1'         TO WRK-NOME-TAB
               PERFORM 1311-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
            END-IF.

            MOVE   DPROCM-ATUAL        OF GFCTB0A1(1:2)
                                       TO WRK-DPROCM-DIA.
            MOVE   DPROCM-ATUAL        OF GFCTB0A1(4:2)
                                       TO WRK-DPROCM-MES.
            MOVE   DPROCM-ATUAL        OF GFCTB0A1(7:4)
                                       TO WRK-DPROCM-ANO.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FORMATA O ERRO SQL                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       1311-MOVER-ERRO-SQL             SECTION.
      *----------------------------------------------------------------*

           MOVE 9                      TO GFCTOI-ERRO.
           MOVE 'GFCT5144'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.
           MOVE SQLCODE                TO GFCT0M-SQLCODE
                                          GFCTOI-COD-SQL-ERRO.

           MOVE SPACES                 TO GFCTOI-DESC-MSG-ERRO.

           PERFORM 1210-OBTER-DESC-MSG.

           STRING  GFCTG3-DESC-MSG     ' - ' WRK-NOME-TAB
           DELIMITED BY '  '           INTO GFCTOI-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1311-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAMENTO PRINCIPAL                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-ABRIR-CURSOR-GFCTB048.

           PERFORM 2200-LER-CURSOR-GFCTB048.

           PERFORM 2300-PROCESSA-CURSOR-GFCTB048
               UNTIL WRK-CSR01-FIM-GFCTB048
                                       EQUAL 'S'.

           PERFORM 2400-FECHAR-CURSOR-GFCTB048.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRIR CURSOR PARA GFCTB048.                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-ABRIR-CURSOR-GFCTB048      SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO IND-1.

           IF  GFCTOH-PONT-PACOTE      EQUAL ZEROS
               MOVE GFCTOH-FILTRO-PACOTE
                                       TO CSERVC-TARIF OF GFCTB048
           ELSE
               MOVE GFCTOH-PONT-PACOTE TO CSERVC-TARIF OF GFCTB048
           END-IF.

           IF  GFCTOH-FILTRO-AGPTO     EQUAL ZEROS
               MOVE GFCTOH-PONT-AGPTO  TO CAGPTO-CTA   OF GFCTB048
           ELSE
               MOVE GFCTOH-FILTRO-AGPTO
                                       TO CAGPTO-CTA   OF GFCTB048
           END-IF.

           MOVE GFCTOH-PONT-SEQ        TO CSEQ-AGPTO-CTA  OF GFCTB048.

           EXEC SQL
                OPEN  CSR01-GFCTB048
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'EXCEC_ADSAO_COMP' TO GFCT0M-NOME-TAB
               MOVE 'OPEN  '           TO GFCT0M-COMANDO-SQL
               MOVE  '0020'            TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTOI-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB048'         TO WRK-NOME-TAB
               PERFORM 1311-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE 'N'                    TO WRK-CSR01-FIM-GFCTB048.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FAZ A LEITURA DE GFCTB048.                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-LER-CURSOR-GFCTB048        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH  CSR01-GFCTB048 INTO
                   :GFCTB048.CSERVC-TARIF,
                   :GFCTB048.CAGPTO-CTA,
                   :GFCTB048.CSEQ-AGPTO-CTA,
                   :GFCTB048.DINIC-ADSAO-COMP,
                   :GFCTB048.CJUNC-DEPDC,
                   :GFCTB048.CCTA-CLI,
                   :GFCTB048.HINCL-EXCEC-ADSAO,
                   :GFCTB048.CFUNC-INCL-EXCEC,
                   :GFCTB048.CFUNC-EXCL-EXCEC,
                   :GFCTB048.HEXCL-EXCEC-ADSAO,
                   :GFCTB048.CINDCD-EXCL-REG,
                   :GFCTB048.DINIC-EXCEC-ADSAO,
                   :GFCTB048.DFIM-EXCEC-ADSAO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'EXEC_ADSAO_COMP'  TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTOI-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB048'         TO WRK-NOME-TAB
               PERFORM 1311-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-CSR01-FIM-GFCTB048
                                          GFCTOI-FIM
               PERFORM 2210-VE-VAZIO
           ELSE
               IF  CSEQ-AGPTO-CTA OF GFCTB048
                                       EQUAL WRK-SEQUENCIA
                   GO                  TO 2200-LER-CURSOR-GFCTB048
               ELSE
                   MOVE CSEQ-AGPTO-CTA OF GFCTB048
                                       TO WRK-SEQUENCIA
               END-IF
            END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      ******************************************************************
      *TRATAMENTO PARA CURSOR VAZIO                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-VE-VAZIO                   SECTION.
      *----------------------------------------------------------------*

           IF  IND-1                   EQUAL 1
               MOVE 1                  TO GFCTOI-ERRO
               MOVE 0907               TO GFCTOI-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               MOVE ZEROS              TO GFCTOI-COD-SQL-ERRO
               PERFORM 1210-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTOI-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      ******************************************************************
      *PROCESSA CURSOR GFCTB048.                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-PROCESSA-CURSOR-GFCTB048  SECTION.
      *----------------------------------------------------------------*

           EVALUATE  GFCTOH-FILTRO-AGPTO
               WHEN 15
                   PERFORM 2310-CONSULTAR-PAB
               WHEN 14
                   PERFORM 2320-CONSULTAR-AGENCIA
               WHEN 17
                   PERFORM 2330-CONS-POSTAL-EXPRE
               WHEN 20
                   PERFORM 2330-CONS-POSTAL-EXPRE
               WHEN 4
                   PERFORM 2340-CONSULTAR-CLIENTE
               WHEN 9
                   PERFORM 2350-CONSULTAR-SEGMENTO
               WHEN 18
                   PERFORM 2360-CONSULTAR-MUNICIPIO
               WHEN 19
                   PERFORM 2370-CONSULTAR-UF
           END-EVALUATE.

           MOVE CSEQ-AGPTO-CTA OF GFCTB048
                                       TO GFCTOI-SEQUENCIA(IND-1).

           ADD 87                      TO GFCTOI-LL.
           ADD 1                       TO GFCTOI-QTDE-TOT-REG.
           ADD 1                       TO IND-1.

           PERFORM 2200-LER-CURSOR-GFCTB048.

           IF  (WRK-CSR01-FIM-GFCTB048 EQUAL 'N') AND
               (IND-1                  GREATER 8)
                MOVE 'S'               TO WRK-CSR01-FIM-GFCTB048
                MOVE CSERVC-TARIF      OF GFCTB048
                                       TO GFCTOI-PONT-PACOTE
                MOVE CAGPTO-CTA        OF GFCTB048
                                       TO GFCTOI-PONT-AGPTO
                MOVE CSEQ-AGPTO-CTA    OF GFCTB048
                                       TO GFCTOI-PONT-SEQ
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSULTA PAB.                                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2310-CONSULTAR-PAB              SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB048
                                       TO CSERVC-TARIF     OF GFCTB005.
           MOVE CAGPTO-CTA             OF GFCTB048
                                       TO CAGPTO-CTA       OF GFCTB005.
           MOVE DINIC-ADSAO-COMP       OF GFCTB048
                                       TO DINIC-ADSAO-COMP OF GFCTB005.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB048
                                       TO CSEQ-AGPTO-CTA   OF GFCTB005.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_ADSAO_COMP,
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
             INTO
                   :GFCTB005.CSERVC-TARIF,
                   :GFCTB005.CAGPTO-CTA,
                   :GFCTB005.CSEQ-AGPTO-CTA,
                   :GFCTB005.DINIC-ADSAO-COMP,
                   :GFCTB005.CEMPR-INC,
                   :GFCTB005.CDEPDC,
                   :GFCTB005.CPOSTO-SERVC
             FROM   DB2PRD.ADSAO_GRP_PAB
             WHERE
                   CSERVC_TARIF       = :GFCTB005.CSERVC-TARIF    AND
                   CAGPTO_CTA         = :GFCTB005.CAGPTO-CTA      AND
                   CSEQ_AGPTO_CTA     = :GFCTB005.CSEQ-AGPTO-CTA  AND
                   DINIC_ADSAO_COMP   = :GFCTB005.DINIC-ADSAO-COMP
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE SPACES             TO GFCTOI-DADOS-RST
               MOVE 'ADSAO_GRP_PAB  '  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTOI-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB005'         TO WRK-NOME-TAB
               PERFORM 1311-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.
           MOVE CPOSTO-SERVC           OF GFCTB005
                                       TO GFCTOI-POSTO(IND-1).

           IF  CPOSTO-SERVC            OF GFCTB005
                                       EQUAL ZEROS
               MOVE 'TODOS'            TO GFCTOI-DESC-NOME(IND-1)
           ELSE
               PERFORM 2311-OBTER-DESC-PAB
               MOVE GFCTGC-DESC-POSTO(1)
                                       TO GFCTOI-DESC-NOME(IND-1)
           END-IF.

           MOVE CDEPDC OF GFCTB005     TO GFCTOI-AGENCIA(IND-1).

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTEM DESCRICAO PAB.                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2311-OBTER-DESC-PAB             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGB-ENTRADA.

           MOVE 100                    TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.

           MOVE GFCTOH-TRANSACAO       TO GFCTGB-TRANSACAO.
           MOVE GFCTOH-FUNCAO          TO GFCTGB-FUNCAO.
           MOVE ZEROS                  TO GFCTGB-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTGB-QTDE-TOT-REG.
           MOVE GFCTOH-FUNC-BDSCO      TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTGB-FIM.

           MOVE CDEPDC       OF GFCTB005
                                       TO GFCTGB-COD-DEPDC.
           MOVE CPOSTO-SERVC OF GFCTB005
                                       TO GFCTGB-COD-POSTO.

           MOVE SPACES                 TO GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCTGC-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5109'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTGB-ENTRADA
                                             GFCTGC-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTOI-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                       WRK-NOME-MODULO
               MOVE WRK-MSG005         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5144'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTGC-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTOI-FIM
               MOVE GFCTGC-ERRO        TO GFCTOI-ERRO
               MOVE GFCTGC-COD-SQL-ERRO
                                       TO GFCTOI-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO
                                       TO GFCTOI-COD-MSG-ERRO
               IF  GFCTGC-ERRO         EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO
                                       TO GFCTOI-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5109-'  GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTOI-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
            END-IF.

      *----------------------------------------------------------------*
       2311-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      ******************************************************************
      *CONSULTA A AGENCIA.                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2320-CONSULTAR-AGENCIA          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB048
                                       TO CSERVC-TARIF     OF GFCTB002.
           MOVE CAGPTO-CTA             OF GFCTB048
                                       TO CAGPTO-CTA       OF GFCTB002.
           MOVE DINIC-ADSAO-COMP       OF GFCTB048
                                       TO DINIC-ADSAO-COMP OF GFCTB002.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB048
                                       TO CSEQ-AGPTO-CTA   OF GFCTB002.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_ADSAO_COMP,
                   CEMPR_INC,
                   CDEPDC
             INTO
                   :GFCTB002.CSERVC-TARIF,
                   :GFCTB002.CAGPTO-CTA,
                   :GFCTB002.CSEQ-AGPTO-CTA,
                   :GFCTB002.DINIC-ADSAO-COMP,
                   :GFCTB002.CEMPR-INC,
                   :GFCTB002.CDEPDC
             FROM   DB2PRD.ADSAO_GRP_DEPDC
             WHERE
                   CSERVC_TARIF       = :GFCTB002.CSERVC-TARIF    AND
                   CAGPTO_CTA         = :GFCTB002.CAGPTO-CTA      AND
                   CSEQ_AGPTO_CTA     = :GFCTB002.CSEQ-AGPTO-CTA  AND
                   DINIC_ADSAO_COMP   = :GFCTB002.DINIC-ADSAO-COMP
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE SPACES             TO GFCTOI-DADOS-RST
               MOVE 'ADSAO_GRP_DPEDC'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTOI-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB002'         TO WRK-NOME-TAB
               PERFORM 1311-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE CDEPDC OF GFCTB002     TO GFCTOI-AGENCIA(IND-1).

           MOVE SPACES                 TO GFCT1V-ENTRADA.

           INITIALIZE                  GFCT1V-ENTRADA.

           MOVE CDEPDC OF GFCTB002     TO GFCT1V-COD-DEPDC(1).

           PERFORM 2321-OBTER-DESC-DEPDC.

           MOVE GFCT1X-DESC-DEPDC(1)   TO GFCTOI-DESC-NOME(IND-1).

      *----------------------------------------------------------------*
       2320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTEM DESCRICAO DEPDC.                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2321-OBTER-DESC-DEPDC           SECTION.
      *----------------------------------------------------------------*

           MOVE 680                    TO GFCT1V-LL.
           MOVE ZEROS                  TO GFCT1V-ZZ.

           MOVE GFCTOH-TRANSACAO       TO GFCT1V-TRANSACAO.
           MOVE GFCTOH-FUNCAO          TO GFCT1V-FUNCAO.
           MOVE 001                    TO GFCT1V-QTDE-OCOR.
           MOVE ZEROS                  TO GFCT1V-QTDE-TOT-REG.
           MOVE GFCTOH-FUNC-BDSCO      TO GFCT1V-FUNC-BDSCO.
           MOVE 'N'                    TO GFCT1V-FIM.

           MOVE SPACES                 TO GFCT1X-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCT1X-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5006'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCT1V-ENTRADA
                                             GFCT1X-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTOI-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                       WRK-NOME-MODULO
               MOVE WRK-MSG005         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5144'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCT1X-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTOI-FIM
               MOVE GFCT1X-ERRO        TO GFCTOI-ERRO
               MOVE GFCT1X-COD-SQL-ERRO
                                       TO GFCTOI-COD-SQL-ERRO
               MOVE GFCT1X-COD-MSG-ERRO
                                       TO GFCTOI-COD-MSG-ERRO
               IF  GFCT1X-ERRO         EQUAL 1
                   MOVE GFCT1X-DESC-MSG-ERRO
                                       TO GFCTOI-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5006-'  GFCT1X-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTOI-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
            END-IF.

      *----------------------------------------------------------------*
       2321-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSULTA POSTAL.                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2330-CONS-POSTAL-EXPRE          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB048
                                       TO CSERVC-TARIF     OF GFCTB006.
           MOVE CAGPTO-CTA             OF GFCTB048
                                       TO CAGPTO-CTA       OF GFCTB006.
           MOVE DINIC-ADSAO-COMP       OF GFCTB048
                                       TO DINIC-ADSAO-COMP OF GFCTB006.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB048
                                       TO CSEQ-AGPTO-CTA   OF GFCTB006.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_ADSAO_COMP,
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
             INTO
                   :GFCTB006.CSERVC-TARIF,
                   :GFCTB006.CAGPTO-CTA,
                   :GFCTB006.CSEQ-AGPTO-CTA,
                   :GFCTB006.DINIC-ADSAO-COMP,
                   :GFCTB006.CEMPR-INC,
                   :GFCTB006.CDEPDC,
                   :GFCTB006.CPOSTO-SERVC
             FROM   DB2PRD.ADSAO_GRP_PSTAL
             WHERE
                   CSERVC_TARIF       = :GFCTB006.CSERVC-TARIF    AND
                   CAGPTO_CTA         = :GFCTB006.CAGPTO-CTA      AND
                   CSEQ_AGPTO_CTA     = :GFCTB006.CSEQ-AGPTO-CTA  AND
                   DINIC_ADSAO_COMP   = :GFCTB006.DINIC-ADSAO-COMP
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE SPACES             TO GFCTOI-DADOS-RST
               MOVE 'ADSAO_GRP_POSTAL' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTOI-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB006'         TO WRK-NOME-TAB
               PERFORM 1311-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE CDEPDC  OF GFCTB006    TO GFCTOI-AGENCIA(IND-1).

           MOVE CPOSTO-SERVC   OF GFCTB006
                                       TO GFCTOI-POSTO(IND-1).

           IF  CDEPDC  OF GFCTB006
                                       EQUAL ZEROS
               MOVE 'TODOS'            TO GFCTOI-DESC-NOME(IND-1)
           ELSE
               IF CPOSTO-SERVC OF GFCTB006
                                           EQUAL ZEROS
                   MOVE 'TODOS'        TO GFCTOI-DESC-NOME(IND-1)
               ELSE
                   PERFORM 2331-OBTER-DESC-POSTAL
                   MOVE WABD-DESC-POSTO(1)
                                       TO GFCTOI-DESC-NOME(IND-1)
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTER DESCRICAO POSTAL.                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2331-OBTER-DESC-POSTAL          SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WABC-ENTRADA.

           MOVE 100                    TO WABC-LL.
           MOVE ZEROS                  TO WABC-ZZ.

           MOVE GFCTOH-TRANSACAO       TO WABC-TRANSACAO.
           MOVE GFCTOH-FUNCAO          TO WABC-FUNCAO.
           MOVE ZEROS                  TO WABC-QTDE-OCOR.
           MOVE ZEROS                  TO WABC-QTDE-TOT-REG.
           MOVE GFCTOH-FUNC-BDSCO      TO WABC-FUNC-BDSCO.
           MOVE 'N'                    TO WABC-FIM.

           MOVE CDEPDC                 OF GFCTB006
                                       TO WABC-COD-DEPDC.
           MOVE CPOSTO-SERVC           OF GFCTB006
                                       TO WABC-COD-POSTO.
           MOVE CAGPTO-CTA             OF GFCTB006
                                       TO WABC-COD-AGRUP.

           MOVE SPACES                 TO WABD-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  WABD-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5209'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WABC-ENTRADA
                                             WABD-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTOI-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                       WRK-NOME-MODULO
               MOVE WRK-MSG005         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5144'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WABD-ERRO             NOT EQUAL ZEROS
               MOVE 'S'              TO GFCTOI-FIM
               MOVE WABD-ERRO        TO GFCTOI-ERRO
               MOVE WABD-COD-SQL-ERRO
                                     TO GFCTOI-COD-SQL-ERRO
               MOVE WABD-COD-MSG-ERRO
                                     TO GFCTOI-COD-MSG-ERRO
               IF  WABD-ERRO         EQUAL 1
                   MOVE WABD-DESC-MSG-ERRO
                                     TO GFCTOI-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5209-'  WABD-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTOI-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
            END-IF.

      *----------------------------------------------------------------*
       2331-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      ******************************************************************
      *CONSULTA CLIENTE.                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2340-CONSULTAR-CLIENTE          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB048
                                       TO CSERVC-TARIF     OF GFCTB0H9.
           MOVE CAGPTO-CTA             OF GFCTB048
                                       TO CAGPTO-CTA       OF GFCTB0H9.
           MOVE DINIC-ADSAO-COMP       OF GFCTB048
                                       TO DINIC-ADSAO-COMP OF GFCTB0H9.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB048
                                       TO CSEQ-AGPTO-CTA   OF GFCTB0H9.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                    DINIC_ADSAO_COMP,
                    CCGC_CPF,
                    CCTRL_CPF_CGC,
                    CFLIAL_CGC,
ST25X6*            Inclusão dos novos campos -ST no SELECT
                    CCGC_CPF_ST,
                    CCTRL_CPF_CGC_ST,
                    CFLIAL_CGC_ST
             INTO
                   :GFCTB0H9.CSERVC-TARIF,
                   :GFCTB0H9.CAGPTO-CTA,
                   :GFCTB0H9.CSEQ-AGPTO-CTA,
                    :GFCTB0H9.DINIC-ADSAO-COMP,
                    :GFCTB0H9.CCGC-CPF,
                    :GFCTB0H9.CCTRL-CPF-CGC,
                    :GFCTB0H9.CFLIAL-CGC,
ST25X6*            Inclusão dos novos hosts -ST no INTO
                    :GFCTB0H9.CCGC-CPF-ST,
                    :GFCTB0H9.CCTRL-CPF-CGC-ST,
                    :GFCTB0H9.CFLIAL-CGC-ST
             FROM   DB2PRD.TADSAO_GRP_CLI
             WHERE
                   CSERVC_TARIF       = :GFCTB0H9.CSERVC-TARIF    AND
                   CAGPTO_CTA         = :GFCTB0H9.CAGPTO-CTA      AND
                   CSEQ_AGPTO_CTA     = :GFCTB0H9.CSEQ-AGPTO-CTA  AND
                   DINIC_ADSAO_COMP   = :GFCTB0H9.DINIC-ADSAO-COMP
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE SPACES             TO GFCTOI-DADOS-RST
               MOVE 'ADSAO_GRP_CLI  ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTOI-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0H9'         TO WRK-NOME-TAB
               PERFORM 1311-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

ST25X6*    MOVE CCGC-CPF               OF GFCTB0H9
ST25X6*                                TO GFCTOI-CPF-CNPJ(IND-1).
ST25X6*    MOVE CCTRL-CPF-CGC          OF GFCTB0H9
ST25X6*                                TO GFCTOI-CONTROLE(IND-1).
ST25X6*    MOVE CFLIAL-CGC             OF GFCTB0H9
ST25X6*                                TO GFCTOI-FILIAL(IND-1).
ST25X6*    Ajuste MOVEs para usar campos -ST
           MOVE CCGC-CPF-ST            OF GFCTB0H9
                                       TO GFCTOI-CPF-CNPJ(IND-1).
           MOVE CCTRL-CPF-CGC-ST       OF GFCTB0H9
                                       TO GFCTOI-CONTROLE(IND-1).
           MOVE CFLIAL-CGC-ST          OF GFCTB0H9
                                       TO GFCTOI-FILIAL(IND-1).

      *----------------------------------------------------------------*
       2340-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSULTA SEGMENTO.                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2350-CONSULTAR-SEGMENTO         SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB048
                                       TO CSERVC-TARIF     OF GFCTB007.
           MOVE CAGPTO-CTA             OF GFCTB048
                                       TO CAGPTO-CTA       OF GFCTB007.
           MOVE DINIC-ADSAO-COMP       OF GFCTB048
                                       TO DINIC-ADSAO-COMP OF GFCTB007.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB048
                                       TO CSEQ-AGPTO-CTA   OF GFCTB007.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_ADSAO_COMP,
                   CSGMTO_GSTAO_TARIF
             INTO
                   :GFCTB007.CSERVC-TARIF,
                   :GFCTB007.CAGPTO-CTA,
                   :GFCTB007.CSEQ-AGPTO-CTA,
                   :GFCTB007.DINIC-ADSAO-COMP,
                   :GFCTB007.CSGMTO-GSTAO-TARIF
                     :WRK-CSGMTO-GSTAO-TARIF-NULL
             FROM   DB2PRD.ADSAO_GRP_SGMTO
             WHERE
                   CSERVC_TARIF       = :GFCTB007.CSERVC-TARIF    AND
                   CAGPTO_CTA         = :GFCTB007.CAGPTO-CTA      AND
                   CSEQ_AGPTO_CTA     = :GFCTB007.CSEQ-AGPTO-CTA  AND
                   DINIC_ADSAO_COMP   = :GFCTB007.DINIC-ADSAO-COMP
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE SPACES             TO GFCTOI-DADOS-RST
               MOVE 'ADSAO_GRP_SEGTO'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0080'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTOI-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB007'         TO WRK-NOME-TAB
               PERFORM 1311-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE CSGMTO-GSTAO-TARIF     OF GFCTB007
                                       TO GFCTOI-SEGMENTO(IND-1).

           PERFORM 2351-OBTER-DESC-SEGMENTO.

           MOVE ISGMTO-GSTAO-TARIF     OF GFCTB0H5
                                       TO GFCTOI-DESC-NOME(IND-1).

      *----------------------------------------------------------------*
       2350-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTER DESCRICAO DO SEGMENTO.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2351-OBTER-DESC-SEGMENTO        SECTION.
      *----------------------------------------------------------------*

           MOVE CSGMTO-GSTAO-TARIF     OF GFCTB007
                                       TO CSGMTO-GSTAO-TARIF
                                                           OF GFCTB0H5.
           EXEC SQL
             SELECT
                   ISGMTO_GSTAO_TARIF
             INTO
                   :GFCTB0H5.ISGMTO-GSTAO-TARIF
             FROM   DB2PRD.TTPO_SGMTO_GSTAO
             WHERE
                   CSGMTO_GSTAO_TARIF = :GFCTB0H5.CSGMTO-GSTAO-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'TIPO_SGMTO_GSTAO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0090'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTOI-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0H5'         TO WRK-NOME-TAB
               PERFORM 1311-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2351-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSULTA O MUNICIPIO.                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2360-CONSULTAR-MUNICIPIO        SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB048
                                       TO CSERVC-TARIF     OF GFCTB004.
           MOVE CAGPTO-CTA             OF GFCTB048
                                       TO CAGPTO-CTA       OF GFCTB004.
           MOVE DINIC-ADSAO-COMP       OF GFCTB048
                                       TO DINIC-ADSAO-COMP OF GFCTB004.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB048
                                       TO CSEQ-AGPTO-CTA   OF GFCTB004.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_ADSAO_COMP,
                   CMUN_IBGE
             INTO
                   :GFCTB004.CSERVC-TARIF,
                   :GFCTB004.CAGPTO-CTA,
                   :GFCTB004.CSEQ-AGPTO-CTA,
                   :GFCTB004.DINIC-ADSAO-COMP,
                   :GFCTB004.CMUN-IBGE
             FROM   DB2PRD.ADSAO_GRP_MUN
             WHERE
                   CSERVC_TARIF       = :GFCTB004.CSERVC-TARIF   AND
                   CAGPTO_CTA         = :GFCTB004.CAGPTO-CTA     AND
                   CSEQ_AGPTO_CTA     = :GFCTB004.CSEQ-AGPTO-CTA AND
                   DINIC_ADSAO_COMP   = :GFCTB004.DINIC-ADSAO-COMP
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE SPACES             TO GFCTOI-DADOS-RST
               MOVE SPACES             TO GFCTOI-DADOS-RST
               MOVE 'ADSAO_GRP_MUN   ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0100'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTOI-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB004'         TO WRK-NOME-TAB
               PERFORM 1311-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE CMUN-IBGE OF GFCTB004  TO GFCTOI-MUNICIPIO(IND-1).

           PERFORM 2361-OBTER-DESC-MUNICIPIO.

           MOVE NOME-MUNIC-RURC88      TO GFCTOI-DESC-NOME(IND-1).

      *----------------------------------------------------------------*
       2360-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTEM DESCRICAO DO MUNICIPIO.                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2361-OBTER-DESC-MUNICIPIO       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE  AREA-RURC88.

           MOVE CMUN-IBGE OF GFCTB004  TO COD-MUNIC-RURC88.

           MOVE 'RURC9020'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             AREA-RURC88.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTOI-ERRO
               MOVE ZEROS              TO GFCTOI-COD-SQL-ERRO
               MOVE 0732               TO GFCTOI-COD-MSG-ERRO
               MOVE '0732'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTOI-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 10
               MOVE 1                  TO GFCTOI-ERRO
               MOVE ZEROS              TO GFCTOI-COD-SQL-ERRO
               MOVE 0675               TO GFCTOI-COD-MSG-ERRO
               MOVE '0675'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTOI-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 20
               MOVE 1                  TO GFCTOI-ERRO
               MOVE ZEROS              TO GFCTOI-COD-SQL-ERRO
               MOVE 0733               TO GFCTOI-COD-MSG-ERRO
               MOVE '0733'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTOI-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
            END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 99
               MOVE 1                  TO GFCTOI-ERRO
               MOVE SQLCODE-RURC88     TO GFCTOI-COD-SQL-ERRO
               MOVE 0734               TO GFCTOI-COD-MSG-ERRO
               MOVE '0734'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTOI-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
            END-IF.

      *----------------------------------------------------------------*
       2361-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FAZ A CONSULTA DE UF.                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2370-CONSULTAR-UF               SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB048
                                       TO CSERVC-TARIF     OF GFCTB008.
           MOVE CAGPTO-CTA             OF GFCTB048
                                       TO CAGPTO-CTA       OF GFCTB008.
           MOVE DINIC-ADSAO-COMP       OF GFCTB048
                                       TO DINIC-ADSAO-COMP OF GFCTB008.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB048
                                       TO CSEQ-AGPTO-CTA   OF GFCTB008.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_ADSAO_COMP,
                   CSGL_UF
             INTO
                   :GFCTB008.CSERVC-TARIF,
                   :GFCTB008.CAGPTO-CTA,
                   :GFCTB008.CSEQ-AGPTO-CTA,
                   :GFCTB008.DINIC-ADSAO-COMP,
                   :GFCTB008.CSGL-UF
             FROM   DB2PRD.ADSAO_GRP_UF
             WHERE
                   CSERVC_TARIF       = :GFCTB008.CSERVC-TARIF    AND
                   CAGPTO_CTA         = :GFCTB008.CAGPTO-CTA      AND
                   CSEQ_AGPTO_CTA     = :GFCTB008.CSEQ-AGPTO-CTA  AND
                   DINIC_ADSAO_COMP   = :GFCTB008.DINIC-ADSAO-COMP
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE SPACES             TO GFCTOI-DADOS-RST
               MOVE 'ADSAO_GRP_UF    ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0110'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTOI-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB008'         TO WRK-NOME-TAB
               PERFORM 1311-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE CSGL-UF OF GFCTB008    TO GFCTOI-UF(IND-1).
           PERFORM 2371-OBTER-DESC-UF.
           MOVE IUF OF DCITV002        TO GFCTOI-DESC-NOME(IND-1).

      *----------------------------------------------------------------*
       2370-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTEM DESCRICAO DO UF.                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2371-OBTER-DESC-UF              SECTION.
      *----------------------------------------------------------------*

           MOVE CSGL-UF OF GFCTB008    TO CSGL-UF OF DCITV002.

           MOVE DCITV002               TO WRK-AREA-DCLGEN-DCITV002.

           MOVE 'DCIT8000'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                            WRK-DCIT-UNIDADE-FEDERATIVA
                                            WRK-POOL7100
                                            WRK-SQLCA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTOI-ERRO
               MOVE ZEROS              TO GFCTOI-COD-SQL-ERRO
               MOVE 0735               TO GFCTOI-COD-MSG-ERRO
               MOVE '0735'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTOI-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 1
               MOVE 1                  TO GFCTOI-ERRO
               MOVE ZEROS              TO GFCTOI-COD-SQL-ERRO
               MOVE 0736               TO GFCTOI-COD-MSG-ERRO
               MOVE '0736'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTOI-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 2
               MOVE 1                  TO GFCTOI-ERRO
               MOVE ZEROS              TO GFCTOI-COD-SQL-ERRO
               MOVE 0737               TO GFCTOI-COD-MSG-ERRO
               MOVE '0737'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTOI-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 3
               MOVE 1                  TO GFCTOI-ERRO
               MOVE ZEROS              TO GFCTOI-COD-SQL-ERRO
               MOVE 0738               TO GFCTOI-COD-MSG-ERRO
               MOVE '0738'             TO GFCTG2-COD-MSG
               PERFORM 1210-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTOI-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 4
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'DCITV002'         TO WRK-NOME-TAB
               MOVE 'V01_UF'           TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0120'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTOI-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 9                  TO GFCTOI-ERRO
               MOVE 'GFCT5144'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE WRK-SQLCA          TO SQLCA
                                          GFCT0M-SQLCA-AREA
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTOI-COD-SQL-ERRO
               MOVE SPACES             TO GFCTOI-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE WRK-AREA-DCLGEN-DCITV002
                                       TO DCITV002.

      *----------------------------------------------------------------*
       2371-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHA CURSOR GFCTB048.                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-FECHAR-CURSOR-GFCTB048     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE   CSR01-GFCTB048
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'EXCEC_ADSAO_COMP' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE '           TO GFCT0M-COMANDO-SQL
               MOVE  '0120'            TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTOI-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB048'         TO WRK-NOME-TAB
               PERFORM 1311-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FINALIZA O PROCESSAMENTO E RETORNA AO PROGRAMA CHAMADOR.        *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
