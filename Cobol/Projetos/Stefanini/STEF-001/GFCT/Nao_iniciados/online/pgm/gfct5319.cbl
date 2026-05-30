      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5319.
       AUTHOR. INFOSERVER.
      *================================================================*
      *                   C P M   S I S T E M A S                      *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT5319                                     *
      *    PROGRAMADOR  : INFOSERVER                                   *
      *    ANALISTA CPM : INFOSERVER                                   *
      *    ANALISTA     : INFOSERVER                                   *
      *    DATA         : MARCO/2017                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      GERAR DADOS PARA COMBO DE TARIFAS.                        *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *      DB2                                                       *
      *       TABLE                                INCLUDE/BOOK        *
      *       DB2PRD.SERVC_TARIF_PRINC               GFCTB0D8          *
      *       DB2PRD.PARM_SERVC_TARIF                GFCTB0A2          *
      *       DB2PRD.PARM_DATA_PROCM                 GFCTB0A1          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5522 - VERIFICACAO DE DISPONIBILIDADE ON-LINE.        *
      *      GFCT5523 - OBTER DESCRICAO DE MENSAGENS                   *
      *      POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                 *
      *      POOL1470 - CONSISTE DATAS COM SECULO                      *
      *                                                                *
      *    NAVEGACAO                                                   *
      *      CHAMADO POR: GFCT7747                                     *
      *                                                                *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
        CONFIGURATION                  SECTION.
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
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT5319  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*         INDEXADORES          *'.
      *----------------------------------------------------------------*

       01  IND-1                       PIC 9(03)    COMP-3 VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-FINALIZAR               PIC X(01)           VALUE SPACES.
       01  WRK-ERRO-DATA               PIC X(01)           VALUE SPACES.
       01  WRK-TIPO-VIGENCIA           PIC X(01)           VALUE SPACES.
       01  WRK-MODULO                  PIC X(08)           VALUE SPACES.
       01  WRK-NOME-TAB                PIC X(08)           VALUE SPACES.
       01  WRK-DT-DINIC                PIC 9(08)           VALUE ZEROS.
       01  WRK-DT-REFER                PIC 9(08)           VALUE ZEROS.
       01  WRK-DATA-PONTEIRO           PIC X(10)           VALUE SPACES.
       01  WRK-AUX-TARIFA              PIC 9(05)           VALUE ZEROS.
       01  WRK-TAB                     PIC X(08)           VALUE SPACES.
       01  WRK-CTWASY-QTDE-TOT-REG     PIC 9(06)  COMP-3   VALUE ZEROS.
       01  WRK-COD-JUNCAO-0315-S       PIC S9(005)         VALUE ZEROS.
       01  WRK-NRO-SEQ-X.
           03  WRK-NRO-SEQ             PIC 9(04)           VALUE ZEROS.

       01  WRK-DATA-DD-MM-AAAA.
           03  WRK-DATA-DD             PIC 9(02)           VALUE ZEROS.
           03  FILLER                  PIC X(01)           VALUE '.'.
           03  WRK-DATA-MM             PIC 9(02)           VALUE ZEROS.
           03  FILLER                  PIC X(01)           VALUE '.'.
           03  WRK-DATA-AAAA           PIC 9(04)           VALUE ZEROS.

       01  WRK-DATA-REFER              PIC X(10)           VALUE SPACES.
       01  FILLER                      REDEFINES WRK-DATA-REFER.
           03  WRK-DTREF-DIA           PIC 9(02).
           03  WRK-P1                  PIC X(01).
           03  WRK-DTREF-MES           PIC 9(02).
           03  WRK-P2                  PIC X(01).
           03  WRK-DTREF-ANO           PIC 9(04).

       01  WRK-CAD-DATA.
           03  WRK-CAD-DIA             PIC 9(02)           VALUE ZEROS.
           03  WRK-CAD-MES             PIC 9(02)           VALUE ZEROS.
           03  WRK-CAD-ANO             PIC 9(04)           VALUE ZEROS.

       01  WRK-SINAL                   PIC -9(03)          VALUE ZEROS.
       01  FILLER    REDEFINES WRK-SINAL.
           03  FILLER                  PIC X(01).
           03  WRK-SINAL-S            PIC 9(03).

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA DE MENSAGENS        *'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGENS.
           03  WRK-MSG05.
               05  FILLER              PIC X(26)           VALUE
                   'ERRO NA CHAMADA DO MODULO'.
               05  WRK-MODULO-MSG05    PIC X(08)           VALUE SPACES.
               05  FILLER              PIC X(41)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREA PARA MODULO GFCT5523   *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREA PARA MODULO GFCT5522   *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTFZ'.

       COPY 'I#GFCTG1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA P/ MODULO GFCT5507   *'.
      *----------------------------------------------------------------*

       01  WRK-5507-AREA.
           03  WRK-5507-AREA-ENVIA.
               05  WRK-5507-AMBIENTE   PIC X(001)          VALUE SPACES.
               05  WRK-5507-CJUNC-DEPDC
                                       PIC 9(005)          VALUE ZEROS.
           03  WRK-5507-RECEBE.
               05  WRK-5507-COD-RETORNO
                                       PIC 9(002)          VALUE ZEROS.
               05  WRK-5507-COD-SQL-ERRO
                                       PIC 9(003)          VALUE ZEROS.
               05  WRK-5507-MSG-RETORNO
                                       PIC X(045)          VALUE SPACES.
               05  WRK-5507-IDTFD-MASTER
                                       PIC X(001)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA P/ MODULO SENH0315   *'.
      *----------------------------------------------------------------*

       01  WRK-VERSAO-0315             PIC  X(006)         VALUE
           'VRS001'.
       01  WRK-MENSAGEM-0315.
           03  WRK-COD-MENSAGEM-0315   PIC  9(003)         VALUE ZEROS.
           03  WRK-HIFEN-MENSAGEM-0315 PIC  X(001)         VALUE SPACES.
           03  WRK-TEXTO-MENSAGEM-0315 PIC  X(079)         VALUE SPACES.
       01  WRK-COD-FUNCIONAL-0315      PIC  X(007)         VALUE SPACES.
       01  WRK-NOME-FUNC-0315          PIC  X(040)         VALUE SPACES.
       01  WRK-COD-BANCO-0315          PIC  9(005)         VALUE ZEROS.
       01  WRK-NOME-BANCO-0315         PIC  X(040)         VALUE SPACES.
       01  WRK-CNPJ-BANCO-0315.
ST25X6*     03  WRK-CNPJ-PRI-0315       PIC  9(009)         VALUE ZEROS.
ST25X6*     03  WRK-CNPJ-FIL-0315       PIC  9(005)         VALUE ZEROS.
ST25X6*     03  WRK-CNPJ-CON-0315       PIC  9(002)         VALUE ZEROS.
           03  WRK-CNPJ-PRI-0315       PIC  X(009)         VALUE SPACES.
           03  WRK-CNPJ-FIL-0315       PIC  X(004)         VALUE SPACES.
           03  WRK-CNPJ-CON-0315       PIC  X(002)         VALUE SPACES.
       01  WRK-COD-JUNCAO-0315         PIC  9(005)         VALUE ZEROS.
       01  WRK-NOME-DEPTO-0315         PIC  X(040)         VALUE SPACES.
       01  WRK-SECAO-0315              PIC  9(003)         VALUE ZEROS.
       01  WRK-STATUS-0315             PIC  X(001)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREAS DE TABELAS DB2     *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A2
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
               DECLARE CSR01-GFCTB0D8 CURSOR FOR
               SELECT
                       CSERVC_TARIF,
                       RSERVC_TARIF_REDZD,
                       CDEPDC
               FROM    DB2PRD.SERVC_TARIF_PRINC
               WHERE   CSERVC_TARIF      >= :GFCTB0D8.CSERVC-TARIF
               AND     CTPO_SERVC_TARIF   IN (2, 4)
               ORDER   BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
               DECLARE CSR02-GFCTB0A2 CURSOR FOR
               SELECT
                       CSERVC_TARIF
               FROM    DB2PRD.PARM_SERVC_TARIF
               WHERE  (CSERVC_TARIF      >= :GFCTB0A2.CSERVC-TARIF)
                AND   (DINIC_VGCIA_TARIF <= :WRK-DATA-REFER
                AND    DFIM_VGCIA_TARIF  >= :WRK-DATA-REFER)
                AND    CSERVC_TARIF      IN (SELECT
                                                    CSERVC_TARIF
                                             FROM
                                              DB2PRD.SERVC_TARIF_PRINC
                                             WHERE
                                                CTPO_SERVC_TARIF IN
                                                      (2, 4))
               ORDER   BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
               DECLARE CSR03-GFCTB0A2 CURSOR FOR
               SELECT
                       CSERVC_TARIF
               FROM    DB2PRD.PARM_SERVC_TARIF
               WHERE  (CSERVC_TARIF      >= :GFCTB0A2.CSERVC-TARIF)
                AND   (DFIM_VGCIA_TARIF  <  :WRK-DATA-REFER
                OR     DINIC_VGCIA_TARIF >  :WRK-DATA-REFER)
                AND    CSERVC_TARIF      IN (SELECT
                                                    CSERVC_TARIF
                                             FROM
                                              DB2PRD.SERVC_TARIF_PRINC
                                             WHERE
                                                CTPO_SERVC_TARIF IN
                                                      (2, 4))
               ORDER   BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
               DECLARE CSR04-GFCTB0A2 CURSOR FOR
               SELECT
                       CSERVC_TARIF,
                       DINIC_VGCIA_TARIF
               FROM    DB2PRD.PARM_SERVC_TARIF
               WHERE ((CSERVC_TARIF      >= :GFCTB0A2.CSERVC-TARIF
                AND    DINIC_VGCIA_TARIF >= :WRK-DATA-PONTEIRO)
                 OR    CSERVC_TARIF      >  :GFCTB0A2.CSERVC-TARIF)
                AND  ((DINIC_VGCIA_TARIF <= :WRK-DATA-REFER
                AND    DFIM_VGCIA_TARIF  >= :WRK-DATA-REFER)
                 OR   (DINIC_VGCIA_TARIF >  :WRK-DATA-REFER))
                AND    CSERVC_TARIF      IN (SELECT
                                                    CSERVC_TARIF
                                             FROM
                                              DB2PRD.SERVC_TARIF_PRINC
                                             WHERE
                                                CTPO_SERVC_TARIF IN
                                                      (2, 4))
               ORDER   BY CSERVC_TARIF,
                          DINIC_VGCIA_TARIF
           END-EXEC.

           EXEC SQL
               DECLARE CSR05-GFCTB0A2   CURSOR FOR
               SELECT
                       CSERVC_TARIF
               FROM    DB2PRD.PARM_SERVC_TARIF
               WHERE  (CSERVC_TARIF      >= :GFCTB0A2.CSERVC-TARIF)
                AND   (DFIM_VGCIA_TARIF  <  :WRK-DATA-REFER)
                AND    CSERVC_TARIF      IN (SELECT
                                                    CSERVC_TARIF
                                             FROM
                                              DB2PRD.SERVC_TARIF_PRINC
                                             WHERE
                                                CTPO_SERVC_TARIF IN
                                                      (2, 4))
               ORDER   BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
               DECLARE CSR06-GFCTB0A2 CURSOR FOR
               SELECT
                       CSERVC_TARIF
               FROM    DB2PRD.PARM_SERVC_TARIF
               WHERE  (CSERVC_TARIF      >= :GFCTB0A2.CSERVC-TARIF)
                AND   (DINIC_VGCIA_TARIF >  :WRK-DATA-REFER)
                AND    CSERVC_TARIF      IN (SELECT
                                                    CSERVC_TARIF
                                             FROM
                                              DB2PRD.SERVC_TARIF_PRINC
                                             WHERE
                                                CTPO_SERVC_TARIF IN
                                                      (2, 4))
               ORDER   BY CSERVC_TARIF
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*    FIM DA WORKING GFCT5319   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'GFCTWASX'.
       COPY 'GFCTWASY'.
       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE DIVISION              USING CTWASX-ENTRADA
                                             CTWASY-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 1100-INICIALIZA-SAIDA.

           PERFORM 1200-CONSISTIR-DADOS.

           PERFORM 1300-VERIFICAR-ONLINE.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-INICIALIZA-SAIDA           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE CTWASY-SAIDA.

           MOVE +268                   TO CTWASY-LL.
           MOVE ZEROS                  TO CTWASY-ZZ.
           MOVE CTWASX-TRANSACAO       TO CTWASY-TRANSACAO.
           MOVE CTWASX-FUNCAO          TO CTWASY-FUNCAO.

           IF  CTWASX-QTDE-OCOR        NOT NUMERIC
               MOVE ZEROS              TO CTWASY-QTDE-OCOR
           ELSE
               MOVE CTWASX-QTDE-OCOR   TO CTWASY-QTDE-OCOR
           END-IF.

           IF  CTWASX-QTDE-TOT-REG     NOT NUMERIC
               MOVE ZEROS              TO CTWASY-QTDE-TOT-REG
           ELSE
               MOVE CTWASX-QTDE-TOT-REG
                                       TO CTWASY-QTDE-TOT-REG
           END-IF

           MOVE CTWASX-FUNC-BDSCO      TO CTWASY-FUNC-BDSCO.
           MOVE CTWASX-TERMINAL        TO CTWASY-TERMINAL.
           MOVE CTWASX-PONTEIRO-TARIFA
                                       TO CTWASY-PONTEIRO-TARIFA.
           MOVE CTWASX-PONTEIRO-DATA   TO CTWASY-PONTEIRO-DATA.

           MOVE 'N'                    TO CTWASY-FIM.

           MOVE ZEROS                  TO CTWASY-ERRO
                                          CTWASY-COD-SQL-ERRO
                                          CTWASY-COD-MSG-ERRO
                                          GFCTG2-COD-MSG.

           PERFORM 1110-OBTER-DESC-MSG.

           MOVE GFCTG3-DESC-MSG        TO CTWASY-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                     TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE CTWASX-TRANSACAO           TO GFCTG2-TRANSACAO.
           MOVE CTWASX-FUNCAO              TO GFCTG2-FUNCAO.
           MOVE CTWASX-FUNC-BDSCO          TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                        TO GFCTG2-FIM.
           MOVE 'O'                        TO GFCTG2-TIPO-PROC.

           MOVE 'GFCT5523'                 TO WRK-MODULO.

           CALL WRK-MODULO                 USING GFCTG2-ENTRADA
                                                 GFCTG3-SAIDA
                                                 GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               MOVE 9                      TO CTWASY-ERRO
               MOVE WRK-MODULO             TO WRK-MODULO-MSG05
               MOVE WRK-MSG05              TO GFCT0M-TEXTO
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5319'             TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           ELSE
               IF  GFCTG3-ERRO             EQUAL 9
                   MOVE 9                  TO CTWASY-ERRO
                   IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                       MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                       MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                       MOVE 'GFCT5319'     TO GFCT0M-TRANSACAO
                   END-IF

                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

           IF  GFCTG3-ERRO                 EQUAL 1
               MOVE 1                      TO CTWASY-ERRO
               MOVE GFCTG3-COD-SQL-ERRO    TO CTWASY-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO    TO CTWASY-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO   TO CTWASY-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (CTWASX-TRANSACAO        EQUAL SPACES)          OR
              (CTWASX-FUNCAO           EQUAL SPACES)          OR
              (CTWASX-CSERVC-TARIF     IS NOT NUMERIC)        OR
              (CTWASX-QTDE-OCOR        IS NOT NUMERIC)        OR
              (CTWASX-QTDE-TOT-REG     IS NOT NUMERIC)        OR
              (CTWASX-FUNC-BDSCO       EQUAL SPACES)          OR
              (CTWASX-PONTEIRO-TARIFA  IS NOT NUMERIC)        OR
              (CTWASX-TIPO-DATA        EQUAL  0               AND
               CTWASX-PONTEIRO-DATA    EQUAL SPACES)          OR
              (CTWASX-FIM              NOT EQUAL 'S' AND 'N') OR
              (CTWASX-TIPO-VIGENCIA    NOT EQUAL 'V' AND 'N'  AND
                                                 'T' AND 'P'  AND
                                                 'H' AND 'F') OR
              (CTWASX-TIPO-DATA        IS NOT NUMERIC         OR
               CTWASX-TIPO-DATA        NOT EQUAL  0  AND  1   AND
                                                  2  AND  3 )
               MOVE 1                  TO CTWASY-ERRO
               MOVE ZEROS              TO CTWASY-COD-SQL-ERRO
               MOVE 0001               TO CTWASY-COD-MSG-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO CTWASY-FIM
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO CTWASY-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1300-VERIFICAR-ONLINE           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG1-SAIDA.

           INITIALIZE GFCTG1-SAIDA.

           MOVE CTWASX-TRANSACAO           TO GFCTFZ-TRANSACAO.
           MOVE CTWASX-FUNCAO              TO GFCTFZ-FUNCAO.
           MOVE CTWASX-FUNC-BDSCO          TO GFCTFZ-FUNC-BDSCO.
           MOVE CTWASX-FIM                 TO GFCTFZ-FIM.
           MOVE 'GFCT'                     TO GFCTFZ-SISTEMA.

           MOVE 'GFCT5522'                 TO WRK-MODULO.

           CALL WRK-MODULO                 USING GFCTFZ-ENTRADA
                                                 GFCTG1-SAIDA
                                                 GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               MOVE 9                      TO CTWASY-ERRO
               MOVE WRK-MODULO             TO WRK-MODULO-MSG05
               MOVE WRK-MSG05              TO GFCT0M-TEXTO
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5319'             TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           ELSE
               IF  GFCTG1-ERRO             EQUAL 9
                   MOVE 9                  TO CTWASY-ERRO
                   IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                       MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
                       MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                       MOVE 'GFCT5319'     TO GFCT0M-TRANSACAO
                   END-IF

                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

           IF  GFCTG1-CSIT-ONLINE          EQUAL 1
               MOVE 9                      TO CTWASY-ERRO
               MOVE 'SISTEMA INDISPONIVEL' TO GFCT0M-TEXTO
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5319'             TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-OBTER-COD-JUNCAO.

           PERFORM 2200-VERIFICAR-MASTER.

           PERFORM 2300-FORMATAR-DT-REF

           IF  CTWASX-PONTEIRO-TARIFA  EQUAL ZEROS
               MOVE CTWASX-CSERVC-TARIF
                                       TO WRK-AUX-TARIFA
               MOVE '01.01.0001'       TO WRK-DATA-PONTEIRO
           ELSE
               MOVE CTWASX-PONTEIRO-TARIFA
                                       TO WRK-AUX-TARIFA
               MOVE CTWASX-PONTEIRO-DATA
                                       TO WRK-DATA-PONTEIRO
           END-IF.

           EVALUATE CTWASX-TIPO-VIGENCIA
               WHEN 'T'
                   MOVE 0235           TO WRK-NRO-SEQ
                   MOVE 'GFCTB0D8'     TO WRK-TAB
                   PERFORM 2400-TRATAR-CSR-1
               WHEN 'V'
                   MOVE 0236           TO WRK-NRO-SEQ
                   MOVE 'GFCTB0A2'     TO WRK-TAB
                   PERFORM 2500-TRATAR-CSR-2
               WHEN 'N'
                   MOVE 0237           TO WRK-NRO-SEQ
                   MOVE 'GFCTB0A2'     TO WRK-TAB
                   PERFORM 2600-TRATAR-CSR-3
               WHEN 'P'
                   MOVE 0238           TO WRK-NRO-SEQ
                   MOVE 'GFCTB0A2'     TO WRK-TAB
                   PERFORM 2700-TRATAR-CSR-4
               WHEN 'H'
                   MOVE 0239           TO WRK-NRO-SEQ
                   MOVE 'GFCTB0A2'     TO WRK-TAB
                   PERFORM 2800-TRATAR-CSR-5
               WHEN OTHER
                   MOVE 0240           TO WRK-NRO-SEQ
                   MOVE 'GFCTB0A2'     TO WRK-TAB
                   PERFORM 2900-TRATAR-CSR-6
           END-EVALUATE.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-OBTER-COD-JUNCAO           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                     TO WRK-VERSAO-0315
                                              WRK-COD-FUNCIONAL-0315
                                              WRK-NOME-FUNC-0315
                                              WRK-NOME-BANCO-0315
                                              WRK-NOME-DEPTO-0315
                                              WRK-STATUS-0315
                                              WRK-HIFEN-MENSAGEM-0315
                                              WRK-TEXTO-MENSAGEM-0315.
           MOVE ZEROS                      TO WRK-COD-JUNCAO-0315
                                              WRK-COD-BANCO-0315
                                              WRK-SECAO-0315
                                              WRK-COD-MENSAGEM-0315
                                              WRK-CNPJ-PRI-0315
                                              WRK-CNPJ-FIL-0315
                                              WRK-CNPJ-CON-0315.
           MOVE 'VRS001'                   TO WRK-VERSAO-0315.
           MOVE CTWASX-FUNC-BDSCO          TO WRK-COD-FUNCIONAL-0315.

           CALL 'SENH0315'                 USING WRK-VERSAO-0315
                                                 WRK-MENSAGEM-0315
                                                 WRK-COD-FUNCIONAL-0315
                                                 WRK-NOME-FUNC-0315
                                                 WRK-COD-BANCO-0315
                                                 WRK-NOME-BANCO-0315
                                                 WRK-CNPJ-BANCO-0315
                                                 WRK-COD-JUNCAO-0315
                                                 WRK-NOME-DEPTO-0315
                                                 WRK-SECAO-0315
                                                 WRK-STATUS-0315.

           IF  RETURN-CODE                 NOT EQUAL ZEROS AND 04
               MOVE 'S'                    TO CTWASY-FIM
               MOVE 1                      TO CTWASY-ERRO
               MOVE RETURN-CODE            TO CTWASY-COD-SQL-ERRO
               MOVE 0056                   TO CTWASY-COD-MSG-ERRO
               MOVE '0056'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG      TO CTWASY-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  RETURN-CODE                 EQUAL 04
               MOVE 'S'                    TO CTWASY-FIM
               IF  WRK-COD-MENSAGEM-0315   EQUAL 001
                   MOVE 1                  TO CTWASY-ERRO
                   MOVE ZEROS              TO CTWASY-COD-SQL-ERRO
                   MOVE 0057               TO CTWASY-COD-MSG-ERRO
                   MOVE '0057'             TO GFCTG2-COD-MSG
                   PERFORM 1110-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG  TO CTWASY-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF

               IF  WRK-COD-MENSAGEM-0315   EQUAL 154
                   MOVE 1                  TO CTWASY-ERRO
                   MOVE ZEROS              TO CTWASY-COD-SQL-ERRO
                   MOVE 0058               TO CTWASY-COD-MSG-ERRO
                   MOVE '0058'             TO GFCTG2-COD-MSG
                   PERFORM 1110-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG  TO CTWASY-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF

               IF  WRK-COD-MENSAGEM-0315   EQUAL 242
                   MOVE 1                  TO CTWASY-ERRO
                   MOVE ZEROS              TO CTWASY-COD-SQL-ERRO
                   MOVE 0059               TO CTWASY-COD-MSG-ERRO
                   MOVE '0059'             TO GFCTG2-COD-MSG
                   PERFORM 1110-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG  TO CTWASY-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF

               IF  WRK-COD-MENSAGEM-0315   EQUAL 009
                   MOVE ZEROS              TO CTWASY-ERRO
                                              CTWASY-COD-SQL-ERRO
                                              CTWASY-COD-MSG-ERRO
                                              GFCTG2-COD-MSG
                   PERFORM 1110-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG  TO CTWASY-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF

               IF  WRK-COD-MENSAGEM-0315   EQUAL 138
                   MOVE 1                  TO CTWASY-ERRO
                   MOVE ZEROS              TO CTWASY-COD-SQL-ERRO
                   MOVE 0060               TO CTWASY-COD-MSG-ERRO
                   MOVE '0060'             TO GFCTG2-COD-MSG
                   PERFORM 1110-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG  TO CTWASY-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-VERIFICAR-MASTER           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WRK-5507-AREA.

           MOVE 'O'                    TO WRK-5507-AMBIENTE.
           MOVE WRK-COD-JUNCAO-0315    TO WRK-5507-CJUNC-DEPDC.
           MOVE 'GFCT5507'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             WRK-5507-AREA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO CTWASY-ERRO

               MOVE 'GFCT5319'         TO GFCT0M-PROGRAMA
               MOVE 'GFCT5507'         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-5507-COD-RETORNO    NOT EQUAL ZEROS
               MOVE 'S'                    TO CTWASY-FIM
               IF  WRK-5507-COD-RETORNO
                                       EQUAL 01
                   MOVE 1              TO CTWASY-ERRO
                   MOVE ZEROS          TO CTWASY-COD-SQL-ERRO
                                          CTWASY-COD-MSG-ERRO
                   MOVE WRK-5507-MSG-RETORNO
                                       TO CTWASY-DESC-MSG-ERRO
               ELSE
                   IF  WRK-5507-COD-RETORNO
                                       EQUAL 99
                       MOVE 1          TO CTWASY-ERRO
                       MOVE WRK-5507-COD-SQL-ERRO
                                       TO CTWASY-COD-SQL-ERRO
                       MOVE ZEROS      TO CTWASY-COD-MSG-ERRO
                       MOVE WRK-5507-MSG-RETORNO
                                       TO CTWASY-DESC-MSG-ERRO
                   END-IF
               END-IF
               MOVE SPACES             TO CTWASY-DADOS
               INITIALIZE CTWASY-DADOS
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-FORMATAR-DT-REF            SECTION.
      *----------------------------------------------------------------*

           IF  CTWASX-TIPO-DATA        EQUAL ZEROS
               MOVE CTWASX-DATA-REFER  TO WRK-DATA-DD-MM-AAAA
               PERFORM 2310-FORMATAR-DT-WEB
               IF  WRK-ERRO-DATA       EQUAL 'S'
                   MOVE 1              TO CTWASY-ERRO
                   MOVE ZEROS          TO CTWASY-COD-SQL-ERRO
                   MOVE 0125           TO CTWASY-COD-MSG-ERRO
                   MOVE '0125'         TO GFCTG2-COD-MSG
                   MOVE 'S'            TO CTWASY-FIM
                   PERFORM 1110-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG
                                       TO CTWASY-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               ELSE
                   MOVE WRK-DATA-AAAA  TO WRK-DTREF-ANO
                   MOVE WRK-DATA-MM    TO WRK-DTREF-MES
                   MOVE WRK-DATA-DD    TO WRK-DTREF-DIA
                   MOVE '.'            TO WRK-P1
                                          WRK-P2
               END-IF
           ELSE
               PERFORM 2320-OBTER-DT-PROCE
               IF  CTWASX-TIPO-DATA    EQUAL 1
                   MOVE DPROCM-ANTER OF GFCTB0A1
                                       TO WRK-DATA-REFER
               ELSE
                   IF  CTWASX-TIPO-DATA
                                       EQUAL 2
                       MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO WRK-DATA-REFER
                   ELSE
                       MOVE DPROCM-PROX OF GFCTB0A1
                                       TO WRK-DATA-REFER
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2310-FORMATAR-DT-WEB            SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-ERRO-DATA.

           IF  WRK-DATA-DD-MM-AAAA     EQUAL SPACES
               MOVE 'S'                TO WRK-ERRO-DATA
           ELSE
               MOVE WRK-DATA-DD        TO WRK-CAD-DIA
               MOVE WRK-DATA-MM        TO WRK-CAD-MES
               MOVE WRK-DATA-AAAA      TO WRK-CAD-ANO
               CALL 'POOL1470'         USING WRK-CAD-DATA
               IF  RETURN-CODE         NOT EQUAL ZEROS
                   MOVE 'S'            TO WRK-ERRO-DATA
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2320-OBTER-DT-PROCE             SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
               SELECT  DPROCM_ANTER,
                       DPROCM_ATUAL,
                       DPROCM_PROX
               INTO    :GFCTB0A1.DPROCM-ANTER,
                       :GFCTB0A1.DPROCM-ATUAL,
                       :GFCTB0A1.DPROCM-PROX
               FROM    DB2PRD.PARM_DATA_PROCM
               WHERE   CSIST_PRINC     = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SINAL
               MOVE WRK-SINAL-S        TO CTWASY-COD-SQL-ERRO
               MOVE '0001'             TO GFCT0M-LOCAL
               MOVE 0011               TO CTWASY-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A1'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2321-MOVER-ERRO-SQL             SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT5319'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.
           MOVE 9                      TO CTWASY-ERRO.

           PERFORM 1110-OBTER-DESC-MSG.

           PERFORM 2330-CONCATENAR-MSG.

      *----------------------------------------------------------------*
       2321-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2330-CONCATENAR-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO CTWASY-DESC-MSG-ERRO.

           STRING GFCTG3-DESC-MSG ' - '
                  WRK-NOME-TAB
           DELIMITED BY '   '          INTO CTWASY-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       2330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2400-TRATAR-CSR-1               SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.

           PERFORM 2410-ABRIR-CSR-1.

           PERFORM 2420-LER-CSR-1.

           PERFORM VARYING IND-1       FROM 1 BY 1
                   UNTIL   IND-1       GREATER 7 OR
                         WRK-FINALIZAR EQUAL 'S'
               MOVE 'T'                TO WRK-TIPO-VIGENCIA
               PERFORM 2530-MOVER-CAMPOS-TARIFA
               PERFORM 2420-LER-CSR-1
           END-PERFORM.

           IF  WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB0D8
                                       TO CTWASY-PONTEIRO-TARIFA
           END-IF.

           PERFORM 2440-FECHAR-CSR-1.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2410-ABRIR-CSR-1                SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF     OF GFCTB0D8.

           EXEC SQL
               OPEN CSR01-GFCTB0D8
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SERVC_TARIF_PRINC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SINAL
               MOVE WRK-SINAL-S        TO CTWASY-COD-SQL-ERRO
               MOVE '0003'             TO GFCT0M-LOCAL
               MOVE 0011               TO CTWASY-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0B8'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2420-LER-CSR-1                  SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH   CSR01-GFCTB0D8
               INTO    :GFCTB0D8.CSERVC-TARIF,
                       :GFCTB0D8.RSERVC-TARIF-REDZD,
                       :GFCTB0D8.CDEPDC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SERVC_TARIF_PRINC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SINAL
               MOVE WRK-SINAL-S        TO CTWASY-COD-SQL-ERRO
               MOVE '0002'             TO GFCT0M-LOCAL
               MOVE 0006               TO CTWASY-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0D8'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 2421-TRATAR-FIM-CSR
           ELSE
               MOVE WRK-COD-JUNCAO-0315 TO WRK-COD-JUNCAO-0315-S
               IF (WRK-5507-IDTFD-MASTER
                                       EQUAL 'N' AND
                   WRK-COD-JUNCAO-0315-S NOT EQUAL CDEPDC OF GFCTB0D8)
                   GO                  TO 2420-LER-CSR-1
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2421-TRATAR-FIM-CSR             SECTION.
      *----------------------------------------------------------------*

           MOVE 'S'                    TO WRK-FINALIZAR
                                          CTWASY-FIM.

           IF  IND-1                   EQUAL ZEROS
               MOVE ZEROS              TO CTWASY-ERRO
                                          CTWASY-COD-SQL-ERRO
               MOVE WRK-NRO-SEQ        TO CTWASY-COD-MSG-ERRO
               MOVE WRK-NRO-SEQ-X      TO GFCTG2-COD-MSG
               MOVE WRK-TAB            TO WRK-NOME-TAB
               PERFORM 1110-OBTER-DESC-MSG
               PERFORM 2330-CONCATENAR-MSG
           END-IF.

      *----------------------------------------------------------------*
       2421-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2440-FECHAR-CSR-1               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-GFCTB0D8
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SERVC_TARIF_PRINC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SINAL
               MOVE WRK-SINAL-S        TO CTWASY-COD-SQL-ERRO
               MOVE '0003'             TO GFCT0M-LOCAL
               MOVE 0011               TO CTWASY-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0D8'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       2440-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2500-TRATAR-CSR-2               SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.

           PERFORM 2510-ABRIR-CSR-2.

           PERFORM 2520-LER-CSR-2.

           PERFORM                     VARYING IND-1 FROM 1 BY 1
                                       UNTIL IND-1 GREATER 7 OR
                                       WRK-FINALIZAR EQUAL 'S'
               MOVE 'V'                TO WRK-TIPO-VIGENCIA
               PERFORM 2530-MOVER-CAMPOS-TARIFA
               PERFORM 2520-LER-CSR-2
           END-PERFORM.

           IF  WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB0A2
                                       TO CTWASY-PONTEIRO-TARIFA
           END-IF.

           PERFORM 2540-FECHAR-CSR-2.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2510-ABRIR-CSR-2                SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF      OF GFCTB0A2.

           EXEC SQL
               OPEN CSR02-GFCTB0A2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SINAL
               MOVE WRK-SINAL-S        TO CTWASY-COD-SQL-ERRO
               MOVE '0007'             TO GFCT0M-LOCAL
               MOVE 0005               TO CTWASY-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2520-LER-CSR-2                  SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH  CSR02-GFCTB0A2
               INTO   :GFCTB0A2.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SINAL
               MOVE WRK-SINAL-S        TO CTWASY-COD-SQL-ERRO
               MOVE '0005'             TO GFCT0M-LOCAL
               MOVE 0006               TO CTWASY-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 2421-TRATAR-FIM-CSR
           ELSE
               PERFORM 2531-OBTER-DESC-TARIFA
               MOVE WRK-COD-JUNCAO-0315 TO WRK-COD-JUNCAO-0315-S
               IF (WRK-5507-IDTFD-MASTER
                                       EQUAL 'N' AND
                   WRK-COD-JUNCAO-0315-S NOT EQUAL CDEPDC OF GFCTB0D8)
                   GO                  TO 2520-LER-CSR-2
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2530-MOVER-CAMPOS-TARIFA        SECTION.
      *----------------------------------------------------------------*

           ADD 106                     TO CTWASY-LL

           MOVE CSERVC-TARIF OF GFCTB0D8
                                       TO CTWASY-COD-TARIFA(IND-1).
           MOVE RSERVC-TARIF-REDZD OF GFCTB0D8
                                       TO CTWASY-DESC-TARIFA(IND-1).
           MOVE WRK-TIPO-VIGENCIA      TO CTWASY-TIPO-VIGENCIA(IND-1).

           MOVE CTWASY-QTDE-TOT-REG     TO WRK-CTWASY-QTDE-TOT-REG.
           ADD 1                        TO WRK-CTWASY-QTDE-TOT-REG.
           MOVE WRK-CTWASY-QTDE-TOT-REG TO CTWASY-QTDE-TOT-REG.

      *----------------------------------------------------------------*
       2530-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2531-OBTER-DESC-TARIFA          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0A2
                                       TO CSERVC-TARIF OF GFCTB0D8.

           EXEC SQL
               SELECT  CDEPDC,
                       CSERVC_TARIF,
                       RSERVC_TARIF_REDZD
               INTO    :GFCTB0D8.CDEPDC,
                       :GFCTB0D8.CSERVC-TARIF,
                       :GFCTB0D8.RSERVC-TARIF-REDZD
               FROM    DB2PRD.SERVC_TARIF_PRINC
               WHERE   CSERVC_TARIF     = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100 AND
                                                 -811) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SERVC_TARIF_PRINC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SINAL
               MOVE WRK-SINAL-S        TO CTWASY-COD-SQL-ERRO
               MOVE '0004'             TO GFCT0M-LOCAL
               MOVE 0009               TO CTWASY-COD-MSG-ERRO
               MOVE '0009'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0D8'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          CTWASY-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO CTWASY-ERRO
                   MOVE ZEROS          TO CTWASY-COD-SQL-ERRO
                   MOVE 0242           TO CTWASY-COD-MSG-ERRO
                   MOVE '0242'         TO GFCTG2-COD-MSG
                   PERFORM 1110-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG  TO CTWASY-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2531-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2540-FECHAR-CSR-2               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR02-GFCTB0A2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SINAL
               MOVE WRK-SINAL-S        TO CTWASY-COD-SQL-ERRO
               MOVE '0006'             TO GFCT0M-LOCAL
               MOVE 0011               TO CTWASY-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       2540-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2600-TRATAR-CSR-3               SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.

           PERFORM 2610-ABRIR-CSR-3.

           PERFORM 2620-LER-CSR-3.

           PERFORM                     VARYING IND-1 FROM 1 BY 1
                                       UNTIL IND-1 GREATER 7 OR
                                       WRK-FINALIZAR EQUAL 'S'
               MOVE ' '                TO WRK-TIPO-VIGENCIA
               PERFORM 2530-MOVER-CAMPOS-TARIFA
               PERFORM 2620-LER-CSR-3
           END-PERFORM.

           IF  WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB0A2
                                       TO CTWASY-PONTEIRO-TARIFA
           END-IF.

           PERFORM 2630-FECHAR-CSR-3.

      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2610-ABRIR-CSR-3                SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF      OF GFCTB0A2.

           EXEC SQL
               OPEN CSR03-GFCTB0A2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SINAL
               MOVE WRK-SINAL-S        TO CTWASY-COD-SQL-ERRO
               MOVE '0007'             TO GFCT0M-LOCAL
               MOVE 0005               TO CTWASY-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2620-LER-CSR-3                  SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR03-GFCTB0A2
               INTO  :GFCTB0A2.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SINAL
               MOVE WRK-SINAL-S        TO CTWASY-COD-SQL-ERRO
               MOVE '0008'             TO GFCT0M-LOCAL
               MOVE 0006               TO CTWASY-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 2421-TRATAR-FIM-CSR
           ELSE
               PERFORM 2531-OBTER-DESC-TARIFA
               MOVE WRK-COD-JUNCAO-0315 TO WRK-COD-JUNCAO-0315-S
               IF (WRK-5507-IDTFD-MASTER
                                       EQUAL 'N' AND
                   WRK-COD-JUNCAO-0315-S NOT EQUAL CDEPDC OF GFCTB0D8)
                   GO                  TO 2620-LER-CSR-3
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2620-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2630-FECHAR-CSR-3               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR03-GFCTB0A2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SINAL
               MOVE WRK-SINAL-S        TO CTWASY-COD-SQL-ERRO
               MOVE '0009'             TO GFCT0M-LOCAL
               MOVE 0011               TO CTWASY-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       2630-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2700-TRATAR-CSR-4               SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.

           PERFORM 2710-ABRIR-CSR-4.

           PERFORM 2720-LER-CSR-4.

           PERFORM                     VARYING IND-1 FROM 1 BY 1
                                       UNTIL IND-1 GREATER 7 OR
                                       WRK-FINALIZAR EQUAL 'S'
               PERFORM 2730-INVERTER-DATAS
               IF  WRK-DT-DINIC        GREATER WRK-DT-REFER
                   MOVE 'P'            TO WRK-TIPO-VIGENCIA
               ELSE
                   MOVE 'V'            TO WRK-TIPO-VIGENCIA
               END-IF
               PERFORM 2530-MOVER-CAMPOS-TARIFA
               PERFORM 2720-LER-CSR-4
           END-PERFORM.

           IF  WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB0A2
                                       TO CTWASY-PONTEIRO-TARIFA
               MOVE DINIC-VGCIA-TARIF OF GFCTB0A2
                                       TO CTWASY-PONTEIRO-DATA
           END-IF.

           PERFORM 2740-FECHAR-CSR-4.

      *----------------------------------------------------------------*
       2700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2710-ABRIR-CSR-4                SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF     OF GFCTB0A2.

           EXEC SQL
               OPEN CSR04-GFCTB0A2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SINAL
               MOVE WRK-SINAL-S        TO CTWASY-COD-SQL-ERRO
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE 0005               TO CTWASY-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2710-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2720-LER-CSR-4                  SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR04-GFCTB0A2
               INTO :GFCTB0A2.CSERVC-TARIF,
                    :GFCTB0A2.DINIC-VGCIA-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SINAL
               MOVE WRK-SINAL-S        TO CTWASY-COD-SQL-ERRO
               MOVE '0011'             TO GFCT0M-LOCAL
               MOVE 0006               TO CTWASY-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 2421-TRATAR-FIM-CSR
           ELSE
               PERFORM 2531-OBTER-DESC-TARIFA
               MOVE WRK-COD-JUNCAO-0315 TO WRK-COD-JUNCAO-0315-S
               IF (WRK-5507-IDTFD-MASTER
                                       EQUAL 'N' AND
                   WRK-COD-JUNCAO-0315-S NOT EQUAL CDEPDC OF GFCTB0D8)
                   GO                  TO 2720-LER-CSR-4
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2720-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2730-INVERTER-DATAS             SECTION.
      *----------------------------------------------------------------*

           STRING DINIC-VGCIA-TARIF OF GFCTB0A2 (07:04)
                  DINIC-VGCIA-TARIF OF GFCTB0A2 (04:02)
                  DINIC-VGCIA-TARIF OF GFCTB0A2 (01:02)
           DELIMITED BY SIZE           INTO WRK-DT-DINIC.

           STRING WRK-DTREF-ANO
                  WRK-DTREF-MES
                  WRK-DTREF-DIA
           DELIMITED BY SIZE           INTO WRK-DT-REFER.

      *----------------------------------------------------------------*
       2730-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2740-FECHAR-CSR-4               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR04-GFCTB0A2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SINAL
               MOVE WRK-SINAL-S        TO CTWASY-COD-SQL-ERRO
               MOVE '0012'             TO GFCT0M-LOCAL
               MOVE 0011               TO CTWASY-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       2740-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2800-TRATAR-CSR-5               SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.

           PERFORM 2810-ABRIR-CSR-5.

           PERFORM 2820-LER-CSR-5.

           PERFORM                     VARYING IND-1 FROM 1 BY 1
                                       UNTIL IND-1 GREATER 7 OR
                                       WRK-FINALIZAR EQUAL 'S'
               MOVE 'H'                TO WRK-TIPO-VIGENCIA
               PERFORM 2530-MOVER-CAMPOS-TARIFA
               PERFORM 2820-LER-CSR-5
           END-PERFORM.

           IF  WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB0A2
                                       TO CTWASY-PONTEIRO-TARIFA
           END-IF.

           PERFORM 2830-FECHAR-CSR-5.

      *----------------------------------------------------------------*
       2800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2810-ABRIR-CSR-5                SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF     OF GFCTB0A2.

           EXEC SQL
               OPEN CSR05-GFCTB0A2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SINAL
               MOVE WRK-SINAL-S        TO CTWASY-COD-SQL-ERRO
               MOVE '0013'             TO GFCT0M-LOCAL
               MOVE 0005               TO CTWASY-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2810-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2820-LER-CSR-5                  SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR05-GFCTB0A2
               INTO  :GFCTB0A2.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SINAL
               MOVE WRK-SINAL-S        TO CTWASY-COD-SQL-ERRO
               MOVE '0014'             TO GFCT0M-LOCAL
               MOVE 0006               TO CTWASY-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 2421-TRATAR-FIM-CSR
           ELSE
               PERFORM 2531-OBTER-DESC-TARIFA
               MOVE WRK-COD-JUNCAO-0315 TO WRK-COD-JUNCAO-0315-S
               IF (WRK-5507-IDTFD-MASTER
                                       EQUAL 'N' AND
                   WRK-COD-JUNCAO-0315-S NOT EQUAL CDEPDC OF GFCTB0D8)
                   GO                  TO 2820-LER-CSR-5
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2820-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2830-FECHAR-CSR-5               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR05-GFCTB0A2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SINAL
               MOVE WRK-SINAL-S        TO CTWASY-COD-SQL-ERRO
               MOVE '0015'             TO GFCT0M-LOCAL
               MOVE 0011               TO CTWASY-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       2830-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2900-TRATAR-CSR-6               SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.

           PERFORM 2910-ABRIR-CSR-6.

           PERFORM 2920-LER-CSR-6.

           PERFORM                     VARYING IND-1 FROM 1 BY 1
                                       UNTIL IND-1 GREATER 7 OR
                                       WRK-FINALIZAR EQUAL 'S'
               MOVE 'F'                TO WRK-TIPO-VIGENCIA
               PERFORM 2530-MOVER-CAMPOS-TARIFA
               PERFORM 2920-LER-CSR-6
           END-PERFORM.

           IF  WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB0A2
                                       TO CTWASY-PONTEIRO-TARIFA
           END-IF.

           PERFORM 2930-FECHAR-CSR-6.

      *----------------------------------------------------------------*
       2900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2910-ABRIR-CSR-6                SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF     OF GFCTB0A2.

           EXEC SQL
               OPEN CSR06-GFCTB0A2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SINAL
               MOVE WRK-SINAL-S        TO CTWASY-COD-SQL-ERRO
               MOVE '0016'             TO GFCT0M-LOCAL
               MOVE 0005               TO CTWASY-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2910-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2920-LER-CSR-6                  SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR06-GFCTB0A2
               INTO  :GFCTB0A2.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SINAL
               MOVE WRK-SINAL-S        TO CTWASY-COD-SQL-ERRO
               MOVE '0017'             TO GFCT0M-LOCAL
               MOVE 0006               TO CTWASY-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 2421-TRATAR-FIM-CSR
           ELSE
               PERFORM 2531-OBTER-DESC-TARIFA
               MOVE WRK-COD-JUNCAO-0315 TO WRK-COD-JUNCAO-0315-S
               IF (WRK-5507-IDTFD-MASTER
                                       EQUAL 'N' AND
                   WRK-COD-JUNCAO-0315-S NOT EQUAL CDEPDC OF GFCTB0D8)
                   GO                  TO 2920-LER-CSR-6
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2920-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2930-FECHAR-CSR-6               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR06-GFCTB0A2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-SINAL
               MOVE WRK-SINAL-S        TO CTWASY-COD-SQL-ERRO
               MOVE '0018'             TO GFCT0M-LOCAL
               MOVE 0011               TO CTWASY-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       2930-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
