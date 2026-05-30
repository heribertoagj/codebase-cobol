      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5157.
       AUTHOR.     JEFERSON PAULO DALPONTE.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT5157                                    *
      *    PROGRAMADOR.:   JEFERSON PAULO DALPONTE - CPM PATO BRANCO   *
      *    ANALISTA CPM:   ADRIANO LOPES SANT'ANNA - CPM PATO BRANCO   *
      *    ANALISTA....:   MARCELO CREMM           - PROCWORK /GP.50   *
      *    DATA........:   27/04/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   LISTA DE ARGUMENTOS DA SUSPENSAO. LISTAR ATE*
      *      10 OCORRENCIAS NA SAIDA E MONTAR RESTART, CASO HAJA, COM A*
      *      CHAVE DA PROXIMA OCORRENCIA VALIDA DO CURSOR (PROXIMA SE- *
      *      QUENCIA , SE HOUVER).                                     *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                        INCLUDE/BOOK     *
      *                   DB2PRD.PARM_DATA_PROCM        GFCTB0A1       *
      *                   DB2PRD.V01UF                  DCITV002       *
      *                   DB2PRD.SUSP_GRP_CLI           GFCTB0E3       *
      *                   DB2PRD.SUSP_GRP_DEPDC         GFCTB0E4       *
      *                   DB2PRD.SUSP_GRP_MUN           GFCTB0E5       *
      *                   DB2PRD.SUSP_GRP_UF            GFCTB0E6       *
      *                   DB2PRD.SUSP_TARIF_AGPTO       GFCTB0E7       *
      *                   DB2PRD.TSUSP_GRP_PAB          GFCTB0I1       *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTPK - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCTPL - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCTG2 - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5523 *
      *    I#GFCTG3 - AREA DE COMUNICACAO - SAIDA DO MODULO GFCT5523   *
      *    I#GFCTFZ - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5522 *
      *    I#GFCTG1 - AREA DE COMUNICACAO - SAIDA DO MODULO GFCT5522   *
      *    I#GFCTGB - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5109 *
      *                                     ENTRADA DO MODULO GFCT5090 *
      *    I#GFCTGC - AREA DE COMUNICACAO - SAIDA DO MODULO GFCT5109   *
      *                                     SAIDA DO MODULO GFCT5090   *
      *    I#GFCT1V - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5006 *
      *    I#GFCT1X - AREA DE COMUNICACAO - SAIDA DO MODULO GFCT5006   *
      *    I#RURC88 - AREA DE COMUNICACAO COM MODULO RURC9020          *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS                      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5522 - VERIFICAR DISPONIBILIDADE DO ON-LINE.            *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    GFCT5109 - OBTER DESCRICAO PAB.                             *
      *    GFCT5006 - OBTER DESCRICAO DEPDC.                           *
      *    GFCT5090 - OBTER DESCRICAO POSTO.                           *
      *    RURC9020 - OBTER DESCRICAO MUNICIPIO.                       *
      *    DCIT8000 - OBTER DESCRICAO UF.                              *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                  *
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
           '* AREA PARA INDEXADORES *'.
      *---------------------------------------------------------------*

       77  IND-1                       PIC  9(003)         VALUE ZEROS.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *---------------------------------------------------------------*

       77  WRK-DPROCM                  PIC  9(008)         VALUE ZEROS.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-NOME-TAB                PIC  X(011)         VALUE SPACES.
       77  WRK-FIM-CURSOR1             PIC  X(001)         VALUE SPACES.

       01  WRK-DATA-DB2.
           05  WRK-DIA-DB2             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  WRK-MES-DB2             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  WRK-ANO-DB2             PIC  9(004)         VALUE ZEROS.

       01  WRK-DATA-INV.
           05  WRK-ANO-INV             PIC  9(004)         VALUE ZEROS.
           05  WRK-MES-INV             PIC  9(002)         VALUE ZEROS.
           05  WRK-DIA-INV             PIC  9(002)         VALUE ZEROS.

       01  WRK-MSG01.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA AO MODULO '.
           05 WRK-NOME-MOD             PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA PARA MODULO DCIT8000 *'.
      *---------------------------------------------------------------*

       01  WRK-DCIT-UNIDADE-FEDERATIVA.
           03  WRK-AREA-DCLGEN-DCITV002 PIC X(070)         VALUE SPACES.
           03  WRK-DCIT-CODIGO-RETORNO  PIC 9(002)         VALUE ZEROS.

       01  WRK-POOL7100                 PIC X(107)         VALUE SPACES.
       01  WRK-SQLCA                    PIC X(136)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA PARA INCLUDES *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCTFZ'.

       COPY 'I#GFCTG1'.

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

       COPY 'I#RURC88'.

       COPY 'I#GFCT1V'.

       COPY 'I#GFCT1X'.

       COPY 'I#GFCTGB'.

       COPY 'I#GFCTGC'.

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
             INCLUDE GFCTB0I1
           END-EXEC.

           EXEC SQL
             INCLUDE DCITV002
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0E3
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0E4
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0E5
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0E6
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0E7
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA DECLARACAO DE CURSORES *'.
      *---------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR01-GFCTB0E7 CURSOR FOR
               SELECT
                   CSERVC_TARIF,
                   DINIC_SUSP_COBR,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   CFUNC_MANUT,
                   CFUNC_MANUT_INCL,
                   DFIM_SUSP_COBR,
                   CINDCD_LIBRC_COBR,
                   DCOBR_SUSP_LIBRD,
                   RJUSTF_SUSP_COBR,
                   CINDCD_AGPTO_TOT,
                   HMANUT_REG
             FROM  DB2PRD.SUSP_TARIF_AGPTO
             WHERE CSERVC_TARIF       = :GFCTB0E7.CSERVC-TARIF
              AND  CAGPTO_CTA         = :GFCTB0E7.CAGPTO-CTA
              AND  CSEQ_AGPTO_CTA    >= :GFCTB0E7.CSEQ-AGPTO-CTA
             ORDER BY
                   CSEQ_AGPTO_CTA
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTPK'.

       COPY 'I#GFCTPL'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTPK-ENTRADA
                                                      GFCTPL-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *ROTINA INICIAL DO PROGRAMA                                      *
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
      *MONTA AREA DE SAIDA, VERIFICA DISPONIBILIDADE DO SISTEMA E CON- *
      *SISTE DADOS.                                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          GFCTPL-SAIDA.

           INITIALIZE GFCTPL-SAIDA
                      IND-1
                      GFCT0M-AREA-ERROS.

           PERFORM 1100-VERIFICAR-DISP-SISTEMA.

           PERFORM 1200-MONTAR-AREA-SAIDA.

           PERFORM 1300-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICA DISPONIBILIDADE DO SISTEMA                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-VERIFICAR-DISP-SISTEMA     SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT5522'                     TO WRK-MODULO.
           MOVE 'GFCT'                         TO GFCTFZ-SISTEMA.
           MOVE GFCTPK-TRANSACAO               TO GFCTFZ-TRANSACAO.
           MOVE GFCTPK-FUNCAO                  TO GFCTFZ-FUNCAO.
           MOVE GFCTPK-FUNC-BDSCO              TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                            TO GFCTFZ-FIM.

           CALL 'POOL0081'                     USING WRK-MODULO
                                                     GFCTFZ-ENTRADA
                                                     GFCTG1-SAIDA
                                                     GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                     NOT EQUAL ZEROS
               MOVE 9                          TO GFCTPL-ERRO
               MOVE WRK-MODULO                 TO WRK-NOME-MOD
                                                  GFCT0M-PROGRAMA
               MOVE WRK-MSG01                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5157'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO                     EQUAL 9
               IF  GFCT0M-TIPO-ACESSO          NOT EQUAL 'DB2'
                   MOVE GFCTG1-DESC-MSG-ERRO   TO GFCT0M-TEXTO
                   MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               ELSE
                   MOVE GFCTG1-COD-SQL-ERRO    TO GFCTPL-COD-SQL-ERRO
               END-IF
               MOVE WRK-MODULO                 TO GFCT0M-PROGRAMA
               MOVE 'GFCT5157'                 TO GFCT0M-TRANSACAO
               MOVE GFCTG1-ERRO                TO GFCTPL-ERRO
               MOVE GFCTG1-COD-MSG-ERRO        TO GFCTPL-COD-MSG-ERRO
               MOVE 'S'                        TO GFCTPL-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE              EQUAL 1
               MOVE 9                          TO GFCTPL-ERRO
               MOVE 'SISTEMA INDISPONIVEL'     TO GFCT0M-TEXTO
               MOVE WRK-MODULO                 TO GFCT0M-PROGRAMA
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5157'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *MONTA AREA DE SAIDA                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-MONTAR-AREA-SAIDA          SECTION.
      *----------------------------------------------------------------*

           MOVE 170                        TO GFCTPL-LL.
           MOVE ZEROS                      TO GFCTPL-ZZ.
           MOVE GFCTPK-TRANSACAO           TO GFCTPL-TRANSACAO.
           MOVE GFCTPK-FUNCAO              TO GFCTPL-FUNCAO.

           IF  GFCTPK-QTDE-OCOR            NOT NUMERIC
               MOVE ZEROS                  TO GFCTPL-QTDE-OCOR
            ELSE
               MOVE GFCTPK-QTDE-OCOR       TO GFCTPL-QTDE-OCOR
           END-IF.

           IF  GFCTPK-QTDE-TOT-REG         NOT NUMERIC
               MOVE ZEROS                  TO GFCTPL-QTDE-TOT-REG
           ELSE
               MOVE GFCTPK-QTDE-TOT-REG    TO GFCTPL-QTDE-TOT-REG
           END-IF.

           MOVE GFCTPK-FUNC-BDSCO          TO GFCTPL-FUNC-BDSCO.
           MOVE GFCTPK-FILTRO              TO GFCTPL-FILTRO.
           MOVE GFCTPK-PONTEIRO            TO GFCTPL-PONTEIRO.

           MOVE 'N'                        TO GFCTPL-FIM.

           MOVE ZEROS                      TO GFCTPL-ERRO
                                              GFCTPL-COD-SQL-ERRO
                                              GFCTPL-COD-MSG-ERRO.
           MOVE '0000'                     TO GFCTG2-COD-MSG.

           PERFORM 1310-OBTER-DESC-MSG.

           MOVE  GFCTG3-DESC-MSG           TO GFCTPL-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR DADOS                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF  GFCTPK-TRANSACAO        EQUAL LOW-VALUES        OR
               GFCTPK-TRANSACAO        EQUAL SPACES            OR
               GFCTPK-FUNCAO           EQUAL LOW-VALUES        OR
               GFCTPK-FUNCAO           EQUAL SPACES            OR
               GFCTPK-FUNC-BDSCO       EQUAL LOW-VALUES        OR
               GFCTPK-FUNC-BDSCO       EQUAL SPACES            OR
               GFCTPK-PONT-AGPTO       NOT NUMERIC             OR
               GFCTPK-PONT-TARIFA      NOT NUMERIC             OR
               GFCTPK-PONT-SEQ         NOT NUMERIC             OR
               GFCTPK-FILTRO-TARIFA    NOT NUMERIC             OR
               GFCTPK-FILTRO-TARIFA    EQUAL ZEROS             OR
               GFCTPK-FILTRO-AGPTO     EQUAL ZEROS             OR
               GFCTPK-FILTRO-AGPTO     NOT EQUAL 4 AND 14 AND 15 AND
                                       18 AND 19
               MOVE 1                  TO GFCTPL-ERRO
                                          GFCTPL-COD-MSG-ERRO
               MOVE ZEROS              TO GFCTPL-COD-SQL-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTPL-DESC-MSG-ERRO
               MOVE 'S'                TO GFCTPL-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 1320-ACESSAR-GFCTB0A1.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTEM DESCRICAO DA MENSAGEM                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1310-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE GFCTPK-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTPK-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTPK-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                     NOT EQUAL ZEROS OR
               GFCTG3-ERRO                     EQUAL 9
               MOVE 9                          TO GFCTPL-ERRO
               MOVE WRK-MODULO                 TO GFCT0M-PROGRAMA
                                                  WRK-NOME-MOD
               MOVE WRK-MSG01                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5157'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO                     EQUAL 1
               MOVE 1                          TO GFCTPL-ERRO
               MOVE 'S'                        TO GFCTPL-FIM
               MOVE GFCTG3-COD-SQL-ERRO        TO GFCTPL-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO        TO GFCTPL-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO       TO GFCTPL-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA ACESSAR GFCTB0A1 PARA BUSCAR DATA                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1320-ACESSAR-GFCTB0A1           SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ATUAL
             INTO  :GFCTB0A1.DPROCM-ATUAL
             FROM  DB2PRD.PARM_DATA_PROCM
             WHERE CSIST_PRINC         = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0A1'      TO WRK-NOME-TAB
               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTPL-COD-MSG-ERRO
               PERFORM 1321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-DIA-INV.
           MOVE WRK-MES-DB2            TO WRK-MES-INV.
           MOVE WRK-ANO-DB2            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-DPROCM.

      *----------------------------------------------------------------*
       1320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA MOVER ERRO SQL                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       1321-MOVER-ERRO-SQL             SECTION.
      *----------------------------------------------------------------*

           MOVE  9                     TO GFCTPL-ERRO.
           MOVE 'GFCT5157'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE  SQLCA                 TO GFCT0M-SQLCA-AREA.
           MOVE  SQLCODE               TO GFCT0M-SQLCODE
                                          GFCTPL-COD-SQL-ERRO.
           MOVE SPACES                 TO GFCTPL-DESC-MSG-ERRO.

           STRING GFCTG3-DESC-MSG      WRK-NOME-TAB

           DELIMITED BY '  '           INTO GFCTPL-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1321-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAMENTO PRINCIPAL                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-ABRIR-CURSOR1.

           PERFORM 2200-LER-CURSOR1.

           PERFORM 2300-PROCESSAR-CURSOR1  VARYING
                       IND-1 FROM 1 BY 1   UNTIL
                       IND-1               GREATER 10 OR
                       WRK-FIM-CURSOR1     EQUAL 'S'.

           IF  WRK-FIM-CURSOR1             NOT EQUAL 'S'
               MOVE 'N'                    TO GFCTPL-FIM
               MOVE CSERVC-TARIF           OF GFCTB0E7
                                           TO GFCTPL-PONT-TARIFA
               MOVE CAGPTO-CTA             OF GFCTB0E7
                                           TO GFCTPL-PONT-AGPTO
               MOVE CSEQ-AGPTO-CTA         OF GFCTB0E7
                                           TO GFCTPL-PONT-SEQ
           END-IF.

           PERFORM 2400-FECHAR-CURSOR1.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA ABRIR CURSOR1                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-ABRIR-CURSOR1              SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                   TO WRK-FIM-CURSOR1.

           IF  GFCTPK-PONT-TARIFA        EQUAL ZEROS
               MOVE GFCTPK-FILTRO-TARIFA TO CSERVC-TARIF    OF GFCTB0E7
           ELSE
               MOVE GFCTPK-PONT-TARIFA   TO CSERVC-TARIF    OF GFCTB0E7
           END-IF.

           IF GFCTPK-PONT-AGPTO          EQUAL ZEROS
               MOVE GFCTPK-FILTRO-AGPTO  TO CAGPTO-CTA      OF GFCTB0E7
           ELSE
               MOVE GFCTPK-PONT-AGPTO    TO CAGPTO-CTA      OF GFCTB0E7
           END-IF.

           MOVE GFCTPK-PONT-SEQ          TO CSEQ-AGPTO-CTA  OF GFCTB0E7.

           EXEC SQL
               OPEN CSR01-GFCTB0E7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0E7'      TO WRK-NOME-TAB
               MOVE 'SUSP_TARIF_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTPL-COD-MSG-ERRO
               PERFORM 1321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA LEITURA DO CURSOR1                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-LER-CURSOR1                SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR01-GFCTB0E7 INTO
                   :GFCTB0E7.CSERVC-TARIF,
                   :GFCTB0E7.DINIC-SUSP-COBR,
                   :GFCTB0E7.CAGPTO-CTA,
                   :GFCTB0E7.CSEQ-AGPTO-CTA,
                   :GFCTB0E7.CFUNC-MANUT,
                   :GFCTB0E7.CFUNC-MANUT-INCL,
                   :GFCTB0E7.DFIM-SUSP-COBR,
                   :GFCTB0E7.CINDCD-LIBRC-COBR,
                   :GFCTB0E7.DCOBR-SUSP-LIBRD,
                   :GFCTB0E7.RJUSTF-SUSP-COBR,
                   :GFCTB0E7.CINDCD-AGPTO-TOT,
                   :GFCTB0E7.HMANUT-REG
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                    EQUAL 'W'      )
               INITIALIZE                  GFCT0M-ERRO-SQL
               MOVE '0006'                 TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0E7'          TO WRK-NOME-TAB
               MOVE 'SUSP_TARIF_AGPTO'     TO GFCT0M-NOME-TAB
               MOVE 'FETCH'                TO GFCT0M-COMANDO-SQL
               MOVE '0030'                 TO GFCT0M-LOCAL
               MOVE  0006                  TO GFCTPL-COD-MSG-ERRO
               PERFORM 1321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                     NOT EQUAL ZEROS
               MOVE 'S'                    TO WRK-FIM-CURSOR1
                                              GFCTPL-FIM
               IF  IND-1                   EQUAL ZEROS
                   MOVE 1                  TO GFCTPL-ERRO
                   MOVE ZEROS              TO GFCTPL-COD-SQL-ERRO
                   MOVE 918                TO GFCTPL-COD-MSG-ERRO
                   MOVE '0918'             TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE  GFCTG3-DESC-MSG   TO GFCTPL-DESC-MSG-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA PROCESSAR CURSOR1                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-PROCESSAR-CURSOR1          SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTPK-FILTRO-AGPTO
               WHEN 15
                   PERFORM 2310-CONSULTAR-PAB

               WHEN 14
                   PERFORM 2320-CONSULTAR-AGENCIA

               WHEN 4
                   PERFORM 2330-CONSULTAR-CLIENTE

               WHEN 18
                   PERFORM 2340-CONSULTAR-MUNICIPIO

               WHEN 19
                   PERFORM 2350-CONSULTAR-UF

           END-EVALUATE.

           MOVE CSEQ-AGPTO-CTA         OF GFCTB0E7
                                       TO GFCTPL-SEQUENCIA(IND-1).

           MOVE DINIC-SUSP-COBR        OF GFCTB0E7
                                       TO GFCTPL-DINIC    (IND-1).

           ADD 84                      TO GFCTPL-LL.
           ADD 1                       TO GFCTPL-QTDE-TOT-REG.

           PERFORM 2200-LER-CURSOR1.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSULTAR PAB                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2310-CONSULTAR-PAB              SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0E7
                                       TO CSERVC-TARIF     OF GFCTB0I1.
           MOVE CAGPTO-CTA             OF GFCTB0E7
                                       TO CAGPTO-CTA       OF GFCTB0I1.
           MOVE DINIC-SUSP-COBR        OF GFCTB0E7
                                       TO DINIC-SUSP-COBR  OF GFCTB0I1.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0E7
                                       TO CSEQ-AGPTO-CTA   OF GFCTB0I1.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_SUSP_COBR,
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
             INTO
                   :GFCTB0I1.CSERVC-TARIF,
                   :GFCTB0I1.CAGPTO-CTA,
                   :GFCTB0I1.CSEQ-AGPTO-CTA,
                   :GFCTB0I1.DINIC-SUSP-COBR,
                   :GFCTB0I1.CEMPR-INC,
                   :GFCTB0I1.CDEPDC,
                   :GFCTB0I1.CPOSTO-SERVC
             FROM  DB2PRD.TSUSP_GRP_PAB
             WHERE CSERVC_TARIF       = :GFCTB0I1.CSERVC-TARIF
              AND  CAGPTO_CTA         = :GFCTB0I1.CAGPTO-CTA
              AND  CSEQ_AGPTO_CTA     = :GFCTB0I1.CSEQ-AGPTO-CTA
              AND  DINIC_SUSP_COBR    = :GFCTB0I1.DINIC-SUSP-COBR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0I1'      TO WRK-NOME-TAB
               MOVE 'TSUSP_GRP_PAB   ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTPL-COD-MSG-ERRO
               PERFORM 1321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE CPOSTO-SERVC           OF GFCTB0I1
                                       TO GFCTPL-POSTO(IND-1).

??????     IF CPOSTO-SERVC             OF GFCTB0I1
??????                                 EQUAL ZEROS
              MOVE 'TODOS'             TO GFCTPL-DESC-NOME(IND-1)
           ELSE
              PERFORM 2311-OBTER-DESCRICAO-PAB
              MOVE GFCTGC-DESC-POSTO(1) TO GFCTPL-DESC-NOME(IND-1)
           END-IF.

           MOVE CDEPDC                 OF GFCTB0I1
                                       TO GFCTPL-AGENCIA(IND-1).

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA OBTER DESCRICAO                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2311-OBTER-DESCRICAO-PAB        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                       TO GFCTGB-ENTRADA
                                                GFCTGC-SAIDA
                                                GFCT0M-AREA-ERROS.

           INITIALIZE                        GFCTGB-ENTRADA
                                             GFCTGC-SAIDA
                                             GFCT0M-AREA-ERROS.

           MOVE 100                          TO GFCTGB-LL.
           MOVE ZEROS                        TO GFCTGB-ZZ.
           MOVE GFCTPK-TRANSACAO             TO GFCTGB-TRANSACAO.
           MOVE GFCTPK-FUNCAO                TO GFCTGB-FUNCAO.
           MOVE ZEROS                        TO GFCTGB-QTDE-OCOR
                                                GFCTGB-QTDE-TOT-REG.
           MOVE GFCTPK-FUNC-BDSCO            TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                          TO GFCTGB-FIM.
           MOVE CDEPDC           OF GFCTB0I1 TO GFCTGB-COD-DEPDC.
           MOVE CPOSTO-SERVC     OF GFCTB0I1 TO GFCTGB-COD-POSTO.
           MOVE 'GFCT5109'                   TO WRK-MODULO.

           CALL 'POOL0081'                   USING WRK-MODULO
                                                   GFCTGB-ENTRADA
                                                   GFCTGC-SAIDA
                                                   GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                   NOT EQUAL ZEROS
               MOVE 9                        TO WRK-MODULO
               MOVE WRK-MODULO               TO WRK-NOME-MOD
                                                GFCT0M-PROGRAMA
               MOVE WRK-MSG01                TO GFCT0M-TEXTO
               MOVE 'APL'                    TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5109'               TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTGC-ERRO                   NOT EQUAL ZEROS
               MOVE 'S'                      TO GFCTPL-FIM
               MOVE GFCTGC-ERRO              TO GFCTPL-ERRO
               MOVE GFCTGC-COD-SQL-ERRO      TO GFCTPL-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO      TO GFCTPL-COD-MSG-ERRO
               IF  GFCTGC-ERRO               EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO TO GFCTPL-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5109-'        GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE         INTO GFCTPL-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2311-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSULTAR AGENCIA                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2320-CONSULTAR-AGENCIA          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0E7
                                       TO CSERVC-TARIF     OF GFCTB0E4.
           MOVE CAGPTO-CTA             OF GFCTB0E7
                                       TO CAGPTO-CTA       OF GFCTB0E4.
           MOVE DINIC-SUSP-COBR        OF GFCTB0E7
                                       TO DINIC-SUSP-COBR  OF GFCTB0E4.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0E7
                                       TO CSEQ-AGPTO-CTA   OF GFCTB0E4.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   DINIC_SUSP_COBR,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   CEMPR_INC,
                   CDEPDC
             INTO
                   :GFCTB0E4.CSERVC-TARIF,
                   :GFCTB0E4.DINIC-SUSP-COBR,
                   :GFCTB0E4.CAGPTO-CTA,
                   :GFCTB0E4.CSEQ-AGPTO-CTA,
                   :GFCTB0E4.CEMPR-INC,
                   :GFCTB0E4.CDEPDC
             FROM   DB2PRD.SUSP_GRP_DEPDC
             WHERE CSERVC_TARIF        = :GFCTB0E4.CSERVC-TARIF
              AND  CAGPTO_CTA          = :GFCTB0E4.CAGPTO-CTA
              AND  CSEQ_AGPTO_CTA      = :GFCTB0E4.CSEQ-AGPTO-CTA
              AND  DINIC_SUSP_COBR     = :GFCTB0E4.DINIC-SUSP-COBR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0E4'      TO WRK-NOME-TAB
               MOVE 'SUSP_GRP_DEPDC  ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTPL-COD-MSG-ERRO
               PERFORM 1321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE CDEPDC                 OF GFCTB0E4
                                       TO GFCTPL-AGENCIA(IND-1).

           IF  CDEPDC                  OF GFCTB0E4
                                       EQUAL ZEROS
               MOVE 'TODAS'            TO GFCTPL-DESC-NOME(IND-1)
           ELSE
               MOVE CDEPDC             OF GFCTB0E4
                                       TO GFCT1V-COD-DEPDC(1)
               PERFORM 2321-OBTER-DESCRICAO-DEPDC
               MOVE GFCT1X-DESC-DEPDC(1) TO GFCTPL-DESC-NOME(IND-1)
           END-IF.

      *---------------------------------------------------------------*
       2320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA OBTER DESCRICAO DEPDC                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2321-OBTER-DESCRICAO-DEPDC      SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT1V-ENTRADA
                                          GFCT1X-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCT1V-ENTRADA
                                       GFCT1X-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 660                    TO GFCT1V-LL.
           MOVE ZEROS                  TO GFCT1V-ZZ.
           MOVE GFCTPK-TRANSACAO       TO GFCT1V-TRANSACAO.
           MOVE GFCTPK-FUNCAO          TO GFCT1V-FUNCAO.
           MOVE 001                    TO GFCT1V-QTDE-OCOR.
           MOVE ZEROS                  TO GFCT1V-QTDE-TOT-REG.
           MOVE GFCTPK-FUNC-BDSCO      TO GFCT1V-FUNC-BDSCO.
           MOVE 'N'                    TO GFCT1V-FIM.
           MOVE CDEPDC                 OF GFCTB0E4
                                       TO GFCT1V-COD-DEPDC(1).
           MOVE 'GFCT5006'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCT1V-ENTRADA
                                             GFCT1X-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO WRK-MODULO
               MOVE WRK-MODULO         TO WRK-NOME-MOD
                                          GFCT0M-PROGRAMA
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5109'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCT1X-ERRO              NOT EQUAL ZEROS
               MOVE 'S'                 TO GFCTPL-FIM
               MOVE GFCT1X-ERRO         TO GFCTPL-ERRO
               MOVE GFCT1X-COD-SQL-ERRO TO GFCTPL-COD-SQL-ERRO
               MOVE GFCT1X-COD-MSG-ERRO TO GFCTPL-COD-MSG-ERRO
               IF  GFCT1X-ERRO          EQUAL 1
                   MOVE GFCT1X-DESC-MSG-ERRO
                                        TO GFCTPL-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5006-'   GFCT1X-DESC-MSG-ERRO
                   DELIMITED BY SIZE    INTO GFCTPL-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2321-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSULTAR CLIENTE                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2330-CONSULTAR-CLIENTE          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0E7
                                       TO CSERVC-TARIF     OF GFCTB0E3.
           MOVE CAGPTO-CTA             OF GFCTB0E7
                                       TO CAGPTO-CTA       OF GFCTB0E3.
           MOVE DINIC-SUSP-COBR        OF GFCTB0E7
                                       TO DINIC-SUSP-COBR  OF GFCTB0E3.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0E7
                                       TO CSEQ-AGPTO-CTA   OF GFCTB0E3.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   DINIC_SUSP_COBR,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   CCGC_CPF,
                   CFLIAL_CGC,
                   CCTRL_CPF_CGC,
ST25X6             CCGC_CPF_ST,
ST25X6             CFLIAL_CGC_ST,
ST25X6             CCTRL_CPF_CGC_ST
             INTO
                   :GFCTB0E3.CSERVC-TARIF,
                   :GFCTB0E3.DINIC-SUSP-COBR,
                   :GFCTB0E3.CAGPTO-CTA,
                   :GFCTB0E3.CSEQ-AGPTO-CTA,
                   :GFCTB0E3.CCGC-CPF,
                   :GFCTB0E3.CFLIAL-CGC,
                   :GFCTB0E3.CCTRL-CPF-CGC,
ST25X6             :GFCTB0E3.CCGC-CPF-ST,
ST25X6             :GFCTB0E3.CFLIAL-CGC-ST,
ST25X6             :GFCTB0E3.CCTRL-CPF-CGC-ST
             FROM   DB2PRD.SUSP_GRP_CLI
             WHERE CSERVC_TARIF        = :GFCTB0E3.CSERVC-TARIF
              AND  CAGPTO_CTA          = :GFCTB0E3.CAGPTO-CTA
              AND  CSEQ_AGPTO_CTA      = :GFCTB0E3.CSEQ-AGPTO-CTA
              AND  DINIC_SUSP_COBR     = :GFCTB0E3.DINIC-SUSP-COBR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0E3'      TO WRK-NOME-TAB
               MOVE 'SUSP_GRP_CLI    ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTPL-COD-MSG-ERRO
               PERFORM 1321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

ST25X6*    MOVE CCGC-CPF      OF GFCTB0E3 TO GFCTPL-CPF-CNPJ(IND-1).
ST25X6*    MOVE CCTRL-CPF-CGC OF GFCTB0E3 TO GFCTPL-CONTROLE(IND-1).
ST25X6*    MOVE CFLIAL-CGC    OF GFCTB0E3 TO  GFCTPL-FILIAL(IND-1).
ST25X6     MOVE CCGC-CPF-ST   OF GFCTB0E3 TO GFCTPL-CPF-CNPJ(IND-1).
ST25X6     MOVE CCTRL-CPF-CGC-ST OF GFCTB0E3 TO GFCTPL-CONTROLE(IND-1).
ST25X6     MOVE CFLIAL-CGC-ST OF GFCTB0E3 TO  GFCTPL-FILIAL(IND-1).

      *----------------------------------------------------------------*
       2330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSULTAR MUNICIPIO                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2340-CONSULTAR-MUNICIPIO        SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0E7
                                       TO CSERVC-TARIF     OF GFCTB0E5.
           MOVE CAGPTO-CTA             OF GFCTB0E7
                                       TO CAGPTO-CTA       OF GFCTB0E5.
           MOVE DINIC-SUSP-COBR        OF GFCTB0E7
                                       TO DINIC-SUSP-COBR  OF GFCTB0E5.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0E7
                                       TO CSEQ-AGPTO-CTA   OF GFCTB0E5.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_SUSP_COBR,
                   CSEQ_AGPTO_CTA,
                   CMUN_IBGE
             INTO
                   :GFCTB0E5.CSERVC-TARIF,
                   :GFCTB0E5.CAGPTO-CTA,
                   :GFCTB0E5.DINIC-SUSP-COBR,
                   :GFCTB0E5.CSEQ-AGPTO-CTA,
                   :GFCTB0E5.CMUN-IBGE
             FROM   DB2PRD.SUSP_GRP_MUN
             WHERE CSERVC_TARIF        = :GFCTB0E5.CSERVC-TARIF
              AND  CAGPTO_CTA          = :GFCTB0E5.CAGPTO-CTA
              AND  CSEQ_AGPTO_CTA      = :GFCTB0E5.CSEQ-AGPTO-CTA
              AND  DINIC_SUSP_COBR     = :GFCTB0E5.DINIC-SUSP-COBR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0E3'      TO WRK-NOME-TAB
               MOVE 'SUSP_GRP_MUN    ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTPL-COD-MSG-ERRO
               PERFORM 1321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE CMUN-IBGE OF GFCTB0E5  TO GFCTPL-MUNICIPIO(IND-1)

           PERFORM 2341-OBTER-DESC-MUNICIPIO.

           MOVE NOME-MUNIC-RURC88      TO GFCTPL-DESC-NOME(IND-1).

      *----------------------------------------------------------------*
       2340-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA OBTER DESCRICAO MUNICIPIO                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2341-OBTER-DESC-MUNICIPIO       SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO AREA-RURC88.

           INITIALIZE AREA-RURC88.

           MOVE CMUN-IBGE OF GFCTB0E5  TO COD-MUNIC-RURC88.
           MOVE 'RURC9020'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             AREA-RURC88.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTPL-ERRO
               MOVE 'S'                TO GFCTPL-FIM
               MOVE ZEROS              TO GFCTPL-COD-SQL-ERRO
               MOVE 0732               TO GFCTPL-COD-MSG-ERRO
               MOVE '0732'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTPL-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 10
               MOVE 1                  TO GFCTPL-ERRO
               MOVE 'S'                TO GFCTPL-FIM
               MOVE ZEROS              TO GFCTPL-COD-SQL-ERRO
               MOVE 0675               TO GFCTPL-COD-MSG-ERRO
               MOVE '0675'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTPL-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 20
               MOVE 1                  TO GFCTPL-ERRO
               MOVE 'S'                TO GFCTPL-FIM
               MOVE ZEROS              TO GFCTPL-COD-SQL-ERRO
               MOVE 0733               TO GFCTPL-COD-MSG-ERRO
               MOVE '0733'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTPL-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 99
               MOVE 1                  TO GFCTPL-ERRO
               MOVE 'S'                TO GFCTPL-FIM
               MOVE ZEROS              TO GFCTPL-COD-SQL-ERRO
               MOVE 0734               TO GFCTPL-COD-MSG-ERRO
               MOVE '0734'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTPL-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2341-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSULTAR UF                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2350-CONSULTAR-UF               SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0E7
                                       TO CSERVC-TARIF     OF GFCTB0E6.
           MOVE CAGPTO-CTA             OF GFCTB0E7
                                       TO CAGPTO-CTA       OF GFCTB0E6.
           MOVE DINIC-SUSP-COBR        OF GFCTB0E7
                                       TO DINIC-SUSP-COBR  OF GFCTB0E6.
           MOVE CSEQ-AGPTO-CTA         OF GFCTB0E7
                                       TO CSEQ-AGPTO-CTA   OF GFCTB0E6.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   DINIC_SUSP_COBR,
                   CSEQ_AGPTO_CTA,
                   CSGL_UF
             INTO
                   :GFCTB0E6.CSERVC-TARIF,
                   :GFCTB0E6.CAGPTO-CTA,
                   :GFCTB0E6.DINIC-SUSP-COBR,
                   :GFCTB0E6.CSEQ-AGPTO-CTA,
                   :GFCTB0E6.CSGL-UF
             FROM   DB2PRD.SUSP_GRP_UF
             WHERE CSERVC_TARIF        = :GFCTB0E6.CSERVC-TARIF
              AND  CAGPTO_CTA          = :GFCTB0E6.CAGPTO-CTA
              AND  CSEQ_AGPTO_CTA      = :GFCTB0E6.CSEQ-AGPTO-CTA
              AND  DINIC_SUSP_COBR     = :GFCTB0E6.DINIC-SUSP-COBR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0E3'      TO WRK-NOME-TAB
               MOVE 'SUSP_GRP_UF     ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE  0012              TO GFCTPL-COD-MSG-ERRO
               PERFORM 1321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE CSGL-UF OF GFCTB0E6    TO GFCTPL-UF(IND-1).

           PERFORM 2351-OBTER-DESC-UF.

           MOVE IUF     OF DCITV002   TO GFCTPL-DESC-NOME(IND-1).

      *----------------------------------------------------------------*
       2350-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA OBTER DESCRICAO UF                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2351-OBTER-DESC-UF              SECTION.
      *----------------------------------------------------------------*

           INITIALIZE AREA-RURC88.

           MOVE CSGL-UF   OF GFCTB0E6  TO CSGL-UF OF DCITV002.
           MOVE DCITV002               TO WRK-AREA-DCLGEN-DCITV002.
           MOVE 'DCIT8000'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-DCIT-UNIDADE-FEDERATIVA
                                             WRK-POOL7100
                                             WRK-SQLCA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTPL-ERRO
               MOVE 'S'                TO GFCTPL-FIM
               MOVE ZEROS              TO GFCTPL-COD-SQL-ERRO
               MOVE 0735               TO GFCTPL-COD-MSG-ERRO
               MOVE '0735'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTPL-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 1
               MOVE 1                  TO GFCTPL-ERRO
               MOVE 'S'                TO GFCTPL-FIM
               MOVE ZEROS              TO GFCTPL-COD-SQL-ERRO
               MOVE 0736               TO GFCTPL-COD-MSG-ERRO
               MOVE '0736'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTPL-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 2
               MOVE 1                  TO GFCTPL-ERRO
               MOVE 'S'                TO GFCTPL-FIM
               MOVE ZEROS              TO GFCTPL-COD-SQL-ERRO
               MOVE 0737               TO GFCTPL-COD-MSG-ERRO
               MOVE '0737'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTPL-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 3
               MOVE 1                  TO GFCTPL-ERRO
               MOVE 'S'                TO GFCTPL-FIM
               MOVE ZEROS              TO GFCTPL-COD-SQL-ERRO
               MOVE 0738               TO GFCTPL-COD-MSG-ERRO
               MOVE '0738'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTPL-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 4
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - DCITV002'      TO WRK-NOME-TAB
               MOVE 'V01_UF'           TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0270'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTPL-COD-MSG-ERRO
               MOVE  9                 TO GFCTPL-ERRO
               MOVE 'GFCT0126'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE  WRK-SQLCA         TO SQLCA
               MOVE  WRK-SQLCA         TO GFCT0M-SQLCA-AREA
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTPL-COD-SQL-ERRO
               MOVE SPACES             TO GFCTPL-DESC-MSG-ERRO
               STRING GFCTG3-DESC-MSG  WRK-NOME-TAB
               DELIMITED BY '  '       INTO GFCTPL-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE WRK-AREA-DCLGEN-DCITV002 TO DCITV002.

      *----------------------------------------------------------------*
       2351-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA FECHAR CURSOR1                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-FECHAR-CURSOR1             SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR01-GFCTB0E7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0E7'      TO WRK-NOME-TAB
               MOVE 'SUSP_TARIF_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTPL-COD-MSG-ERRO
               PERFORM 1321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA FINALIZAR PROGRAMA                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
