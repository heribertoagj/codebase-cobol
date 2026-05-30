      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0197.
       AUTHOR.     GIORGIA L. B. BORSATTI.
      *================================================================*
      *              C P M  B R A X I S - S I S T E M A S              *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0197                                    *
      *    PROGRAMADOR.:   GIORGIA L. B. BORSATTI  - CPM BRAXIS /FPOLIS*
      *    ANALISTA CPM:   MAURICIO NAPOLEAO       - CPM BRAXIS /FPOLIS*
      *    ANALISTA....:   HELIO SANTONI           - PROCWORK /GP.50   *
      *    DATA........:   15/07/2008                                  *
      *                                                                *
      *    OBJETIVO....:                                               *
      *      FLEXIBILIZACAO - LISTA DE CONSULTA DE CONTAS AGRUPAMENTO  *
      *                       PERSONALIZADO.                           *
      *                                                                *
      *    BCO DE DADOS:                                               *
      *      DB2                                                       *
      *        TABLE                                  INCLUDE/BOOK     *
      *        DB2PRD.TCTA_AGPTO_PERSO                  GFCTB0J5       *
      *        DB2PRD.PARM_DATA_PROCM                   GFCTB0A1       *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5522 - VERIFICAR DISPONIBILIDADE DO ON-LINE           *
      *      GFCT5523 - OBTER DESCRICAO DE MENSAGENS                   *
      *      GFCT5054 - OBTER IDENTIF DO SOLICITANTE DE FLEXIBILIZACAO *
      *                                                                *
BI1211*----------------------------------------------------------------*
BI1211*    ULTIMA ALTERACAO EM 26/12/2011 - BIRA                       *
BI1211*    - AUMENTAR O TAMANHO DOS INDEXADORES PARA SUPORTAR VOLUME   *
BI1211*      MAIOR QUE 999 OCORRENCIAS (PGM EM LOOP).                  *
BI1211*                                                                *
PEROT *================================================================*
  "   *----------------------------------------------------------------*
  "   *                  ALTERACAO                                     *
  "   *  PROGRAMA....: GFCT0197                                        *
  "   *  ANALISTA....: OTAVIO PIMENTEL       -   CAPGEMINI             *
  "   *  DATA DA ALTERACAO: MAIO/2013                                  *
  "   *  DESCRICAO: FOI INSERIDO DOIS CURSORES (CSR03 E CSR04) PARA    *
  "   *             QUANDO A AGENCIA ESTIVER PREENCHIDA PASSAR POR     *
  "   *             IGUALDADE.                                         *
  "   *             ALTERADO CAMPO DOS CURSORES E DO SELECT COUNT PARA *
PEROT *             CBCO = 237.                                        *
VI0618*----------------------------------------------------------------*
VI0618*    ULTIMA ALTERACAO EM 08/06/2018 - VINICIUS CURTO             *
VI0618*    - MOVIMENTA DATA FIM DE VIGENCIA PARA A SAIDA QUANDO        *
VI0618*      CONSULTA DA FUNCIONALIDADE DE CANCELAMENTO.               *
VI0618*                                                                *
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
       77  FILLER                      PIC  X(033)         VALUE
           'INICIO DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(021)         VALUE
           'AREA PARA INDEXADORES'.
      *----------------------------------------------------------------*

BI1211 77  IND-1                       PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(030)         VALUE
           'AREA PARA VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*

BI1211 77  WRK-DESPREZA                PIC  9(009)  COMP-3 VALUE ZEROS.
       77  WRK-QTDE-REGISTROS          PIC S9(009)  COMP   VALUE ZEROS.
       77  WRK-QTDE-REGISTROSA         PIC S9(009)  COMP   VALUE ZEROS.

       77  WRK-CAG-BCRIA-INI           PIC S9(005) COMP-3  VALUE ZEROS.
       77  WRK-CAG-BCRIA-FIM           PIC S9(005) COMP-3  VALUE ZEROS.
       77  WRK-CCTA-BCRIA-CLI-INI      PIC S9(013) COMP-3  VALUE ZEROS.
       77  WRK-CCTA-BCRIA-CLI-FIM      PIC S9(013) COMP-3  VALUE ZEROS.

       77  WRK-RST-COMANDADO           PIC  X(001)         VALUE 'N'.
       77  WRK-CURSOR                  PIC  9(001)         VALUE ZEROS.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-NOME-TAB                PIC  X(011)         VALUE SPACES.
       77  WRK-INCONSISTENTE           PIC  X(001)         VALUE SPACES.
       77  WRK-FINALIZAR               PIC  X(001)         VALUE SPACES.
       77  WRK-GFCTJO-QTDE-TOT-REG     PIC  9(006) COMP-3  VALUE ZEROS.

       01  WRK-MSG001.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA DO MODULO '.
           05 WRK-MODULO-MSG           PIC  X(008)         VALUE SPACES.
           05 FILLER                   PIC  X(041)         VALUE SPACES.

       01  WRK-MSG002                  PIC  X(075)         VALUE
           'SISTEMA INDISPONIVEL'.

       01  WRK-S9-3                    PIC +9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES           WRK-S9-3.
           05  FILLER                  PIC  X(007).
           05  WRK-3-9                 PIC  9(003).

BI1211 01  WRK-QTDE-OCOR               PIC  9(009) COMP-3 VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           'AREA DE COMUNICACAO COM GFCT5522'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTFZ'.

       COPY 'I#GFCTG1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           'AREA DE COMUNICACAO COM GFCT5523'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           'AREA DE COMUNICACAO COM GFCT5054'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG0'.

       COPY 'I#GFCTHA'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(013)         VALUE
           'AREA PARA DB2'.
      *----------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0J5
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
            DECLARE  CSR01-GFCTB0J5  CURSOR FOR
             SELECT  CAG_BCRIA,
                     CCTA_BCRIA_CLI,
                     DINIC_VGCIA_REG,
                     DFIM_VGCIA_REG
               FROM  DB2PRD.TCTA_AGPTO_PERSO
              WHERE  CAGPTO_CTA         = :GFCTB0J5.CAGPTO-CTA
PEROT           AND  CBCO               = 237
                AND  CAG_BCRIA         >= :WRK-CAG-BCRIA-INI
                AND  CAG_BCRIA         <= :WRK-CAG-BCRIA-FIM
                AND  CCTA_BCRIA_CLI    >= :WRK-CCTA-BCRIA-CLI-INI
                AND  CCTA_BCRIA_CLI    <= :WRK-CCTA-BCRIA-CLI-FIM
              ORDER  BY CAG_BCRIA,
                        CCTA_BCRIA_CLI,
                        DINIC_VGCIA_REG
           END-EXEC.

           EXEC SQL
             DECLARE CSR02-GFCTB0J5 CURSOR FOR
             SELECT  CAG_BCRIA,
                     CCTA_BCRIA_CLI,
VI0618               DINIC_VGCIA_REG,
VI0618               DFIM_VGCIA_REG
               FROM  DB2PRD.TCTA_AGPTO_PERSO
              WHERE  CAGPTO_CTA         = :GFCTB0J5.CAGPTO-CTA
PEROT           AND  CBCO               = 237
                AND  CAG_BCRIA         >= :WRK-CAG-BCRIA-INI
                AND  CAG_BCRIA         <= :WRK-CAG-BCRIA-FIM
                AND  CCTA_BCRIA_CLI    >= :WRK-CCTA-BCRIA-CLI-INI
                AND  CCTA_BCRIA_CLI    <= :WRK-CCTA-BCRIA-CLI-FIM
      *         AND ((DINIC_VGCIA_REG  <= :GFCTB0J5.DINIC-VGCIA-REG
      *         AND   DFIM_VGCIA_REG   >= :GFCTB0J5.DFIM-VGCIA-REG)
                AND   DFIM_VGCIA_REG   >  :GFCTB0J5.DFIM-VGCIA-REG
              ORDER  BY CAG_BCRIA,
                        CCTA_BCRIA_CLI,
                        DINIC_VGCIA_REG
           END-EXEC.

PEROT      EXEC SQL
  "         DECLARE  CSR03-GFCTB0J5  CURSOR FOR
  "          SELECT  CAG_BCRIA,
  "                  CCTA_BCRIA_CLI,
  "                  DINIC_VGCIA_REG,
  "                  DFIM_VGCIA_REG
  "            FROM  DB2PRD.TCTA_AGPTO_PERSO
  "           WHERE  CAGPTO_CTA         = :GFCTB0J5.CAGPTO-CTA
  "             AND  CBCO               = 237
  "             AND  CAG_BCRIA          = :WRK-CAG-BCRIA-INI
  "             AND  CCTA_BCRIA_CLI    >= :WRK-CCTA-BCRIA-CLI-INI
  "             AND  CCTA_BCRIA_CLI    <= :WRK-CCTA-BCRIA-CLI-FIM
  "           ORDER  BY CAG_BCRIA,
  "                     CCTA_BCRIA_CLI,
  "                     DINIC_VGCIA_REG
PEROT      END-EXEC.

PEROT      EXEC SQL
  "          DECLARE CSR04-GFCTB0J5 CURSOR FOR
  "          SELECT  CAG_BCRIA,
  "                  CCTA_BCRIA_CLI,
VI0618               DINIC_VGCIA_REG,
VI0618               DFIM_VGCIA_REG
  "            FROM  DB2PRD.TCTA_AGPTO_PERSO
  "           WHERE  CAGPTO_CTA         = :GFCTB0J5.CAGPTO-CTA
  "             AND  CBCO               = 237
  "             AND  CAG_BCRIA          = :WRK-CAG-BCRIA-INI
  "             AND  CCTA_BCRIA_CLI    >= :WRK-CCTA-BCRIA-CLI-INI
  "             AND  CCTA_BCRIA_CLI    <= :WRK-CCTA-BCRIA-CLI-FIM
  "             AND   DFIM_VGCIA_REG   >  :GFCTB0J5.DFIM-VGCIA-REG
  "           ORDER  BY CAG_BCRIA,
  "                     CCTA_BCRIA_CLI,
  "                     DINIC_VGCIA_REG
PEROT      END-EXEC.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(030)         VALUE
           'FIM DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTJN'.

       COPY 'I#GFCTJO'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTJN-ENTRADA
                                                      GFCTJO-SAIDA
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
                                          GFCTJO-SAIDA.

           INITIALIZE GFCTJO-SAIDA
                      GFCT0M-AREA-ERROS
                      WRK-FINALIZAR
                      WRK-INCONSISTENTE
                      WRK-CURSOR
                      IND-1.

           PERFORM 1100-MONTAR-AREA-SAIDA.

           PERFORM 1200-VERIFICAR-DISP-SISTEMA.

           PERFORM 1300-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *MONTA AREA DE SAIDA                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-MONTAR-AREA-SAIDA          SECTION.
      *----------------------------------------------------------------*

           MOVE 1010                  TO GFCTJO-LL.
           MOVE GFCTJN-ZZ             TO GFCTJO-ZZ.
           MOVE GFCTJN-TRANSACAO      TO GFCTJO-TRANSACAO.
           MOVE GFCTJN-FUNCAO         TO GFCTJO-FUNCAO.

           IF  GFCTJN-QTDE-OCOR        NOT NUMERIC
               MOVE ZEROS              TO GFCTJO-QTDE-OCOR
           ELSE
               MOVE GFCTJN-QTDE-OCOR   TO GFCTJO-QTDE-OCOR
           END-IF.

           MOVE ZEROS              TO GFCTJO-QTDE-TOT-REG.

           MOVE GFCTJN-FUNC-BDSCO     TO GFCTJO-FUNC-BDSCO.
           MOVE GFCTJN-PONTEIRO       TO GFCTJO-PONTEIRO.
           MOVE GFCTJN-FILTRO         TO GFCTJO-FILTRO.
           MOVE GFCTJN-TIPO-CONSULTA  TO GFCTJO-TIPO-CONSULTA
           MOVE 'N'                   TO GFCTJO-FIM.
           MOVE ZEROS                 TO GFCTJO-COD-SQL-ERRO
                                         GFCTJO-COD-MSG-ERRO
                                         GFCTJO-ERRO.
           MOVE '0000'                TO GFCTG2-COD-MSG.

           PERFORM 1310-OBTER-DESC-MSG.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICA DISPONIBILIDADE DO SISTEMA                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-VERIFICAR-DISP-SISTEMA     SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG1-SAIDA.

           INITIALIZE GFCTG1-SAIDA.

           MOVE +101                   TO GFCTFZ-LL.
           MOVE ZEROS                  TO GFCTFZ-ZZ.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE GFCTJN-TRANSACAO       TO GFCTFZ-TRANSACAO.
           MOVE GFCTJN-FUNCAO          TO GFCTFZ-FUNCAO.
           MOVE GFCTJN-FUNC-BDSCO      TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.
           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE ZEROS              TO RETURN-CODE
               MOVE SPACES             TO GFCTJO-DESC-MSG-ERRO
               MOVE 9                  TO GFCTJO-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0197'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO             NOT EQUAL ZEROS
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE SPACES         TO GFCTJO-DESC-MSG-ERRO
                   MOVE GFCTG1-DESC-MSG-ERRO(1:75)
                                       TO GFCT0M-TEXTO
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
               ELSE
                   MOVE GFCTG1-COD-SQL-ERRO
                                       TO GFCTJO-COD-SQL-ERRO
                   MOVE GFCTG1-DESC-MSG-ERRO
                                       TO GFCTJO-DESC-MSG-ERRO
               END-IF
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'GFCT0197'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTJO-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 9                  TO GFCTJO-ERRO
               MOVE SPACES             TO GFCTJO-DESC-MSG-ERRO
               MOVE WRK-MSG002         TO GFCT0M-TEXTO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0197'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTE DADOS                                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (GFCTJN-TRANSACAO        EQUAL SPACES OR LOW-VALUES) OR
              (GFCTJN-FUNCAO           EQUAL SPACES OR LOW-VALUES) OR
              (GFCTJN-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES)
           OR (GFCTJN-AGRUPMTO-FIL     NOT NUMERIC)
           OR (GFCTJN-AGENCIA-FIL      NOT NUMERIC)
           OR (GFCTJN-CONTA-FIL        NOT NUMERIC)
           OR (GFCTJN-AGRUPMTO-PONT    NOT NUMERIC)
           OR (GFCTJN-AGENCIA-PONT     NOT NUMERIC)
           OR (GFCTJN-CONTA-PONT       NOT NUMERIC)
               MOVE 1                  TO GFCTJO-ERRO
               MOVE ZEROS              TO GFCTJO-COD-SQL-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               MOVE 0001               TO GFCTJO-COD-MSG-ERRO
               PERFORM 1310-OBTER-DESC-MSG
               PERFORM 3000-FINALIZAR
           END-IF.

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
           MOVE GFCTJN-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTJN-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTJN-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE ZEROS              TO RETURN-CODE
               MOVE SPACES             TO GFCTJO-DESC-MSG-ERRO
               MOVE 9                  TO GFCTJO-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0197'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE SPACES         TO GFCTJO-DESC-MSG-ERRO
                   MOVE GFCTG3-DESC-MSG-ERRO(1:75)
                                       TO GFCT0M-TEXTO
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
               ELSE
                   MOVE GFCTG3-COD-SQL-ERRO
                                       TO GFCTJO-COD-SQL-ERRO
                   MOVE GFCTG3-DESC-MSG-ERRO
                                       TO GFCTJO-DESC-MSG-ERRO
               END-IF
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'GFCT0197'         TO GFCT0M-TRANSACAO
               MOVE GFCTG3-ERRO        TO GFCTJO-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE GFCTG3-ERRO        TO GFCTJO-ERRO
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO GFCTJO-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO GFCTJO-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO GFCTJO-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE GFCTG3-DESC-MSG        TO GFCTJO-DESC-MSG-ERRO
                                         (1: LENGTH OF GFCTG3-DESC-MSG)
                                          GFCT0M-TEXTO
                                         (1: LENGTH OF GFCTG3-DESC-MSG).

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAMENTO PRINCIPAL                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTJN-AGRUPMTO-FIL      TO CAGPTO-CTA     OF GFCTB0J5
           MOVE 237                      TO CBCO           OF GFCTB0J5.

           IF  GFCTJN-AGENCIA-FIL   EQUAL  ZEROS
               MOVE ZEROS                TO WRK-CAG-BCRIA-INI
               MOVE 99999                TO WRK-CAG-BCRIA-FIM
           ELSE
               MOVE GFCTJN-AGENCIA-FIL   TO WRK-CAG-BCRIA-INI
                                            WRK-CAG-BCRIA-FIM
           END-IF.

           IF  GFCTJN-CONTA-FIL     EQUAL  ZEROS
               MOVE ZEROS                TO WRK-CCTA-BCRIA-CLI-INI
               MOVE 9999999999999        TO WRK-CCTA-BCRIA-CLI-FIM
           ELSE
               MOVE GFCTJN-CONTA-FIL     TO WRK-CCTA-BCRIA-CLI-INI
               MOVE 9999999999999        TO WRK-CCTA-BCRIA-CLI-FIM
               MOVE 'S'                  TO WRK-RST-COMANDADO
           END-IF.

      *--> VERIFICAR QTDE DE OCORR. QD SELECIONA SOMENTE AGRUP.
           IF  GFCTJN-AGENCIA-FIL   EQUAL  ZEROS
               PERFORM 2010-OBTER-QTDE-OCORRA
           ELSE
               IF  GFCTJN-CONTA-FIL     EQUAL ZEROS
                   MOVE GFCTJN-AGENCIA-FIL TO CAG-BCRIA OF GFCTB0J5
                   PERFORM 2020-OBTER-QTDE-OCORRC
               END-IF
           END-IF.

           IF  GFCTJN-TIPO-CONSULTA    EQUAL 'H'
               PERFORM 2100-PROCESSAR-HISTORICO
           ELSE
               PERFORM 2200-PROCESSAR-CANCELA
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2010-OBTER-QTDE-OCORRA          SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               SELECT COUNT(*)
                 INTO :WRK-QTDE-REGISTROS
               FROM   DB2PRD.TCTA_AGPTO_PERSO
               WHERE  CAGPTO_CTA        = :GFCTB0J5.CAGPTO-CTA
               AND    CBCO              = 237
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0J5'      TO WRK-NOME-TAB
               MOVE 'GFCT0197'         TO GFCT0M-PROGRAMA
                                          GFCT0M-TRANSACAO
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TCTA_AGPTO_PERSO' TO GFCT0M-NOME-TAB
               MOVE 'OPEN  '           TO GFCT0M-COMANDO-SQL
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-S9-3
               MOVE WRK-3-9            TO GFCTJO-COD-SQL-ERRO
               MOVE 9                  TO GFCTJO-ERRO
               MOVE 0005               TO GFCTJO-COD-MSG-ERRO
               MOVE SPACES             TO GFCTJO-DESC-MSG-ERRO
               PERFORM 2111-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-QTDE-REGISTROS GREATER 150
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTJO-FIM
               MOVE 1                  TO GFCTJO-ERRO
               MOVE ZEROS              TO GFCTJO-COD-SQL-ERRO
               MOVE '0633'             TO GFCTG2-COD-MSG
               MOVE 0633               TO GFCTJO-COD-MSG-ERRO
               PERFORM 1310-OBTER-DESC-MSG
               PERFORM 3000-FINALIZAR
           END-IF.


      *----------------------------------------------------------------*
       2010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2020-OBTER-QTDE-OCORRC          SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               SELECT COUNT(*)
                 INTO :WRK-QTDE-REGISTROSA
               FROM   DB2PRD.TCTA_AGPTO_PERSO
               WHERE  CAGPTO_CTA        = :GFCTB0J5.CAGPTO-CTA
               AND    CAG_BCRIA         = :GFCTB0J5.CAG-BCRIA
               AND    CBCO              = 237
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0J5'      TO WRK-NOME-TAB
               MOVE 'GFCT0197'         TO GFCT0M-PROGRAMA
                                          GFCT0M-TRANSACAO
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TCTA_AGPTO_PERSO' TO GFCT0M-NOME-TAB
               MOVE 'OPEN  '           TO GFCT0M-COMANDO-SQL
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE SQLCA(1:136)       TO GFCT0M-SQLCA-AREA
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-S9-3
               MOVE WRK-3-9            TO GFCTJO-COD-SQL-ERRO
               MOVE 9                  TO GFCTJO-ERRO
               MOVE 0005               TO GFCTJO-COD-MSG-ERRO
               MOVE SPACES             TO GFCTJO-DESC-MSG-ERRO
               PERFORM 2111-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-QTDE-REGISTROSA GREATER 150
               MOVE 'S'             TO  WRK-RST-COMANDADO
           END-IF.


      *----------------------------------------------------------------*
       2020-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR HISTORICO                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-PROCESSAR-HISTORICO        SECTION.
      *----------------------------------------------------------------*
PEROT      IF GFCTJN-AGENCIA-FIL EQUAL ZEROS

              PERFORM 2110-ABRIR-CSR01-GFCTB0J5

              MOVE GFCTJN-QTDE-OCOR TO WRK-DESPREZA

              PERFORM 2120-LER-CSR01-GFCTB0J5

              PERFORM 2120-LER-CSR01-GFCTB0J5 VARYING IND-1
                      FROM 1           BY 1
                      UNTIL IND-1      GREATER WRK-DESPREZA OR
                      WRK-FINALIZAR    EQUAL 'S'

              PERFORM VARYING IND-1    FROM 1 BY 1 UNTIL
                      IND-1            GREATER 10 OR
                      WRK-FINALIZAR    EQUAL 'S'

                  PERFORM 2130-PROCESSAR-HIST

              END-PERFORM

PEROT      ELSE
  "
  "           PERFORM 2121-ABRIR-CSR03-GFCTB0J5
  "
              MOVE GFCTJN-QTDE-OCOR TO WRK-DESPREZA
  "
  "           PERFORM 2122-LER-CSR03-GFCTB0J5
  "
  "           PERFORM 2122-LER-CSR03-GFCTB0J5 VARYING IND-1
  "                   FROM 1           BY 1
  "                   UNTIL IND-1      GREATER WRK-DESPREZA OR
  "                   WRK-FINALIZAR    EQUAL 'S'
  "
  "           PERFORM VARYING IND-1    FROM 1 BY 1 UNTIL
  "                   IND-1            GREATER 10 OR
  "                   WRK-FINALIZAR    EQUAL 'S'
  "
  "               PERFORM 2130-PROCESSAR-HIST
  "
  "           END-PERFORM
  "
PEROT      END-IF.

           IF  IND-1                   GREATER 10  AND
               WRK-FINALIZAR           NOT   EQUAL 'S'
               MOVE CAGPTO-CTA         OF GFCTB0J5
                                       TO GFCTJO-AGRUPMTO-PONT
               MOVE CAG-BCRIA          OF GFCTB0J5
                                       TO GFCTJO-AGENCIA-PONT
               MOVE CCTA-BCRIA-CLI     OF GFCTB0J5
                                       TO GFCTJO-CONTA-PONT
           END-IF.

PEROT      IF GFCTJN-AGENCIA-FIL     EQUAL ZEROS
              PERFORM 2140-FECHAR-CSR01-GFCTB0J5
PEROT      ELSE
  "           PERFORM 2141-FECHAR-CSR03-GFCTB0J5
PEROT      END-IF.
      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRE O CURSOR 1                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-ABRIR-CSR01-GFCTB0J5       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN CSR01-GFCTB0J5
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0J5'      TO WRK-NOME-TAB
               MOVE 'GFCT0197'         TO GFCT0M-PROGRAMA
                                          GFCT0M-TRANSACAO
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TCTA_AGPTO_PERSO' TO GFCT0M-NOME-TAB
               MOVE 'OPEN  '           TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-S9-3
               MOVE WRK-3-9            TO GFCTJO-COD-SQL-ERRO
               MOVE 9                  TO GFCTJO-ERRO
               MOVE 0005               TO GFCTJO-COD-MSG-ERRO
               MOVE SPACES             TO GFCTJO-DESC-MSG-ERRO
               PERFORM 2111-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CONCATENAR MENSAGEM                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2111-CONCATENAR-MENSAGEM        SECTION.
      *----------------------------------------------------------------*

           STRING GFCTG3-DESC-MSG(01:68) WRK-NOME-TAB

           DELIMITED BY '  '           INTO GFCTJO-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       2111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EFETUA LEITURA PELO CURSOR 1                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2120-LER-CSR01-GFCTB0J5         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR01-GFCTB0J5 INTO
                   :GFCTB0J5.CAG-BCRIA,
                   :GFCTB0J5.CCTA-BCRIA-CLI,
                   :GFCTB0J5.DINIC-VGCIA-REG,
                   :GFCTB0J5.DFIM-VGCIA-REG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZERO AND +100) OR
              (SQLWARN                 EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0J5'      TO WRK-NOME-TAB
               MOVE 'GFCT0197'         TO GFCT0M-PROGRAMA
                                          GFCT0M-TRANSACAO
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TCTA_AGPTO_PERSO' TO GFCT0M-NOME-TAB
               MOVE 'FETCH  '          TO GFCT0M-COMANDO-SQL
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-S9-3
               MOVE WRK-3-9            TO GFCTJO-COD-SQL-ERRO
               MOVE 9                  TO GFCTJO-ERRO
               MOVE SPACES             TO GFCTJO-DESC-MSG-ERRO
               MOVE 0006               TO GFCTJO-COD-MSG-ERRO
               PERFORM 2111-CONCATENAR-MENSAGEM
               PERFORM 2140-FECHAR-CSR01-GFCTB0J5
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTJO-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO GFCTJO-ERRO
                   MOVE ZEROS          TO GFCTJO-COD-SQL-ERRO
                   MOVE 0010           TO GFCTJO-COD-MSG-ERRO
                   MOVE '0010'         TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
BI1211             PERFORM 3000-FINALIZAR
               END-IF
           END-IF.


      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      ******************************************************************
      *ABRE O CURSOR 3                                                 *
      ******************************************************************
PEROT *----------------------------------------------------------------*
  "    2121-ABRIR-CSR03-GFCTB0J5       SECTION.
  "   *----------------------------------------------------------------*
  "
  "        EXEC SQL
  "            OPEN CSR03-GFCTB0J5
  "        END-EXEC.
  "
  "        IF (SQLCODE                 NOT EQUAL ZEROS) OR
  "           (SQLWARN0                EQUAL 'W')
  "            MOVE SPACES             TO GFCT0M-ERRO-SQL
  "            INITIALIZE              GFCT0M-ERRO-SQL
  "            MOVE '0005'             TO GFCTG2-COD-MSG
  "            PERFORM 1310-OBTER-DESC-MSG
  "            MOVE ' - GFCTB0J5'      TO WRK-NOME-TAB
  "            MOVE 'GFCT0197'         TO GFCT0M-PROGRAMA
  "                                       GFCT0M-TRANSACAO
  "            MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
  "            MOVE 'TCTA_AGPTO_PERSO' TO GFCT0M-NOME-TAB
  "            MOVE 'OPEN  '           TO GFCT0M-COMANDO-SQL
  "            MOVE '0040'             TO GFCT0M-LOCAL
  "            MOVE SQLCA              TO GFCT0M-SQLCA-AREA
  "            MOVE SQLCODE            TO GFCT0M-SQLCODE
  "                                       WRK-S9-3
  "            MOVE WRK-3-9            TO GFCTJO-COD-SQL-ERRO
  "            MOVE 9                  TO GFCTJO-ERRO
  "            MOVE 0005               TO GFCTJO-COD-MSG-ERRO
  "            MOVE SPACES             TO GFCTJO-DESC-MSG-ERRO
  "            PERFORM 2111-CONCATENAR-MENSAGEM
  "            PERFORM 3000-FINALIZAR
  "        END-IF.
  "
  "   *----------------------------------------------------------------*
  "    2121-99-FIM.                    EXIT.
  "   *----------------------------------------------------------------*
  "   ******************************************************************
  "   *EFETUA LEITURA PELO CURSOR 3                                    *
  "   ******************************************************************
  "   *----------------------------------------------------------------*
  "    2122-LER-CSR03-GFCTB0J5         SECTION.
  "   *----------------------------------------------------------------*
  "
  "        EXEC SQL
  "          FETCH CSR03-GFCTB0J5 INTO
  "                :GFCTB0J5.CAG-BCRIA,
  "                :GFCTB0J5.CCTA-BCRIA-CLI,
  "                :GFCTB0J5.DINIC-VGCIA-REG,
  "                :GFCTB0J5.DFIM-VGCIA-REG
  "        END-EXEC.
  "
  "        IF (SQLCODE                 NOT EQUAL ZERO AND +100) OR
  "           (SQLWARN                 EQUAL 'W')
  "            MOVE SPACES             TO GFCT0M-ERRO-SQL
  "            INITIALIZE              GFCT0M-ERRO-SQL
  "            MOVE '0006'             TO GFCTG2-COD-MSG
  "            PERFORM 1310-OBTER-DESC-MSG
  "            MOVE ' - GFCTB0J5'      TO WRK-NOME-TAB
  "            MOVE 'GFCT0197'         TO GFCT0M-PROGRAMA
  "                                       GFCT0M-TRANSACAO
  "            MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
  "            MOVE 'TCTA_AGPTO_PERSO' TO GFCT0M-NOME-TAB
  "            MOVE 'FETCH  '          TO GFCT0M-COMANDO-SQL
  "            MOVE '0050'             TO GFCT0M-LOCAL
  "            MOVE SQLCA              TO GFCT0M-SQLCA-AREA
  "            MOVE SQLCODE            TO GFCT0M-SQLCODE
  "                                       WRK-S9-3
  "            MOVE WRK-3-9            TO GFCTJO-COD-SQL-ERRO
  "            MOVE 9                  TO GFCTJO-ERRO
  "            MOVE SPACES             TO GFCTJO-DESC-MSG-ERRO
  "            MOVE 0006               TO GFCTJO-COD-MSG-ERRO
  "            PERFORM 2111-CONCATENAR-MENSAGEM
  "            PERFORM 2141-FECHAR-CSR03-GFCTB0J5
  "            PERFORM 3000-FINALIZAR
  "        END-IF.
  "
  "        IF  SQLCODE                 EQUAL +100
  "            MOVE 'S'                TO WRK-FINALIZAR
  "                                       GFCTJO-FIM
  "            IF  IND-1               EQUAL ZEROS
  "                MOVE 1              TO GFCTJO-ERRO
  "                MOVE ZEROS          TO GFCTJO-COD-SQL-ERRO
  "                MOVE 0010           TO GFCTJO-COD-MSG-ERRO
  "                MOVE '0010'         TO GFCTG2-COD-MSG
  "                PERFORM 1310-OBTER-DESC-MSG
  "                PERFORM 3000-FINALIZAR
  "            END-IF
  "        END-IF.

 "
  "   *----------------------------------------------------------------*
  "    2122-99-FIM.                    EXIT.
PEROT *----------------------------------------------------------------*

      ******************************************************************
      *  MONTA AS OCORRENCIAS NA AREA DE SAIDA                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2130-PROCESSAR-HIST             SECTION.
      *----------------------------------------------------------------*

           MOVE CAG-BCRIA              OF GFCTB0J5
                                       TO GFCTJO-AGENCIA  (IND-1).
           MOVE CCTA-BCRIA-CLI         OF GFCTB0J5
                                       TO GFCTJO-CONTA    (IND-1).
           MOVE DINIC-VGCIA-REG        OF GFCTB0J5
                                       TO GFCTJO-DATA-INIC(IND-1).

           MOVE DFIM-VGCIA-REG         OF GFCTB0J5
                                       TO GFCTJO-DATA-FIM (IND-1).

           PERFORM 2131-PESQUISAR-NM-CLIENTE.

           MOVE GFCTJO-QTDE-TOT-REG    TO WRK-GFCTJO-QTDE-TOT-REG.
BI1211     MOVE GFCTJO-QTDE-OCOR       TO WRK-QTDE-OCOR.

           ADD 1                       TO WRK-GFCTJO-QTDE-TOT-REG
                                          WRK-QTDE-OCOR
           MOVE WRK-GFCTJO-QTDE-TOT-REG
                                       TO GFCTJO-QTDE-TOT-REG.
           MOVE WRK-QTDE-OCOR          TO GFCTJO-QTDE-OCOR

           IF WRK-RST-COMANDADO        EQUAL 'S'
              IF  WRK-QTDE-OCOR        EQUAL 150
                   MOVE 'S'            TO WRK-FINALIZAR
                                          GFCTJO-FIM
                   GO TO               2130-99-FIM
               END-IF
           END-IF.

PEROT      IF GFCTJN-AGENCIA-FIL     EQUAL ZEROS
              PERFORM 2120-LER-CSR01-GFCTB0J5
PEROT      ELSE
  "           PERFORM 2122-LER-CSR03-GFCTB0J5
PEROT      END-IF.

      *----------------------------------------------------------------*
       2130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  PESQUISAR NOME DO CLIENTE                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2131-PESQUISAR-NM-CLIENTE       SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG0-ENTRADA
                                          GFCTHA-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCTG0-ENTRADA
                                       GFCTHA-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE +100                   TO GFCTG0-LL.
           MOVE ZEROS                  TO GFCTG0-ZZ.
           MOVE GFCTJN-TRANSACAO       TO GFCTG0-TRANSACAO.
           MOVE GFCTJN-FUNCAO          TO GFCTG0-FUNCAO.
           MOVE GFCTJN-FUNC-BDSCO      TO GFCTG0-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG0-FIM.

           MOVE CAG-BCRIA              TO GFCTG0-COD-DEPDC.
           MOVE CCTA-BCRIA-CLI         TO GFCTG0-CCTA-CLI.

           MOVE 'GFCT5054'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTG0-ENTRADA
                                             GFCTHA-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTJO-ERRO
               MOVE SPACES             TO GFCTJO-DESC-MSG-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
                                         (1:LENGTH OF WRK-MSG001)
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0197'         TO GFCT0M-TRANSACAO
               IF  GFCTJN-TIPO-CONSULTA
                                       EQUAL 'H'
                   PERFORM 2140-FECHAR-CSR01-GFCTB0J5
               ELSE
                   PERFORM 2250-FECHAR-CSR02-GFCTB0J5
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTHA-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTJO-FIM
               MOVE GFCTHA-ERRO        TO GFCTJO-ERRO
               MOVE GFCTHA-COD-SQL-ERRO
                                       TO GFCTJO-COD-SQL-ERRO
               MOVE GFCTHA-COD-MSG-ERRO
                                       TO GFCTJO-COD-MSG-ERRO
               IF  GFCTHA-ERRO         EQUAL 1
                   MOVE GFCTHA-DESC-MSG-ERRO
                                       TO GFCTJO-DESC-MSG-ERRO
               ELSE
                   MOVE 'GFCT0197'     TO GFCT0M-TRANSACAO
                   STRING 'GFCT5054 - ' GFCTHA-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTJO-DESC-MSG-ERRO
               END-IF
               IF  GFCTJN-TIPO-CONSULTA
                                       EQUAL 'H'
                   PERFORM 2140-FECHAR-CSR01-GFCTB0J5
               ELSE
                   PERFORM 2250-FECHAR-CSR02-GFCTB0J5
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE GFCTHA-NOME-CLIENTE (1:30) TO GFCTJO-NOME (IND-1).

      *----------------------------------------------------------------*
       2131-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHA O CURSOR 1                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2140-FECHAR-CSR01-GFCTB0J5      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR01-GFCTB0J5
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0J5'      TO WRK-NOME-TAB
               MOVE 'GFCT0197'         TO GFCT0M-PROGRAMA
                                          GFCT0M-TRANSACAO
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TCTA_AGPTO_PERSO' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE   '         TO GFCT0M-COMANDO-SQL
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-S9-3
               MOVE WRK-3-9            TO GFCTJO-COD-SQL-ERRO
               MOVE 9                  TO GFCTJO-ERRO
               MOVE SPACES             TO GFCTJO-DESC-MSG-ERRO
               MOVE 0011               TO GFCTJO-COD-MSG-ERRO
               PERFORM 2111-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
PEROT ******************************************************************
  "   *FECHA O CURSOR 3                                                *
  "   ******************************************************************
  "   *----------------------------------------------------------------*
  "    2141-FECHAR-CSR03-GFCTB0J5      SECTION.
  "   *----------------------------------------------------------------*
  "
  "        EXEC SQL
  "          CLOSE CSR03-GFCTB0J5
  "        END-EXEC.
  "
  "        IF (SQLCODE                 NOT EQUAL ZEROS) OR
  "           (SQLWARN0                EQUAL 'W')
  "            MOVE SPACES             TO GFCT0M-ERRO-SQL
  "            INITIALIZE GFCT0M-ERRO-SQL
  "            MOVE '0011'             TO GFCTG2-COD-MSG
  "            PERFORM 1310-OBTER-DESC-MSG
  "            MOVE ' - GFCTB0J5'      TO WRK-NOME-TAB
  "            MOVE 'GFCT0197'         TO GFCT0M-PROGRAMA
  "                                       GFCT0M-TRANSACAO
  "            MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
  "            MOVE 'TCTA_AGPTO_PERSO' TO GFCT0M-NOME-TAB
  '            MOVE 'CLOSE   '         TO GFCT0M-COMANDO-SQL
  "            MOVE '0070'             TO GFCT0M-LOCAL
  "            MOVE SQLCA              TO GFCT0M-SQLCA-AREA
  "            MOVE SQLCODE            TO GFCT0M-SQLCODE
  "                                       WRK-S9-3
  "            MOVE WRK-3-9            TO GFCTJO-COD-SQL-ERRO
  "            MOVE 9                  TO GFCTJO-ERRO
  "            MOVE SPACES             TO GFCTJO-DESC-MSG-ERRO
  "            MOVE 0011               TO GFCTJO-COD-MSG-ERRO
  "            PERFORM 2111-CONCATENAR-MENSAGEM
  "            PERFORM 3000-FINALIZAR
  "        END-IF.
  "
  "   *----------------------------------------------------------------*
  "    2141-99-FIM.                    EXIT.
PEROT *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR COMBO SELECIONADO                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-PROCESSAR-CANCELA          SECTION.
      *----------------------------------------------------------------*

           PERFORM 2210-OBTER-DATA-PROCESSAMENTO.

PEROT      IF GFCTJN-AGENCIA-FIL EQUAL ZEROS

              PERFORM 2220-ABRIR-CSR02-GFCTB0J5

              MOVE GFCTJN-QTDE-OCOR    TO WRK-DESPREZA

              PERFORM 2230-LER-CSR02-GFCTB0J5

              PERFORM 2230-LER-CSR02-GFCTB0J5 VARYING IND-1
                      FROM 1           BY 1
                      UNTIL IND-1      GREATER WRK-DESPREZA OR
                      WRK-FINALIZAR    EQUAL 'S'

              PERFORM VARYING IND-1    FROM 1 BY 1 UNTIL
                      IND-1            GREATER 10 OR
                      WRK-FINALIZAR    EQUAL 'S'

               PERFORM 2240-PROCESSAR-SEL

              END-PERFORM

PEROT      ELSE
  "
  "           PERFORM 2221-ABRIR-CSR04-GFCTB0J5
  "
  "           MOVE GFCTJN-QTDE-OCOR    TO WRK-DESPREZA
  "
  "           PERFORM 2231-LER-CSR04-GFCTB0J5
  "
  "           PERFORM 2231-LER-CSR04-GFCTB0J5 VARYING IND-1
  "                   FROM 1           BY 1
  "                   UNTIL IND-1      GREATER WRK-DESPREZA OR
  "                   WRK-FINALIZAR    EQUAL 'S'
  "
  "           PERFORM VARYING IND-1    FROM 1 BY 1 UNTIL
  "                   IND-1            GREATER 10 OR
  "                   WRK-FINALIZAR    EQUAL 'S'
  "
  "           PERFORM 2240-PROCESSAR-SEL
  "
  "           END-PERFORM
  "
PEROT      END-IF.

           IF IND-1                   GREATER 10 AND
              WRK-FINALIZAR           NOT EQUAL 'S'
              MOVE CAGPTO-CTA         OF GFCTB0J5
                                      TO GFCTJO-AGRUPMTO-PONT
              MOVE CAG-BCRIA          OF GFCTB0J5
                                      TO GFCTJO-AGENCIA-PONT
              MOVE CCTA-BCRIA-CLI     OF GFCTB0J5
                                       TO GFCTJO-CONTA-PONT
           END-IF.

PEROT      IF GFCTJN-AGENCIA-FIL     EQUAL ZEROS
  "           PERFORM 2250-FECHAR-CSR02-GFCTB0J5
  "        ELSE
  "           PERFORM 2251-FECHAR-CSR04-GFCTB0J5
PEROT      END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER DATA DO PROCESSAMENTO ATUAL - GFCTB0A1.                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-OBTER-DATA-PROCESSAMENTO   SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
             SELECT  DPROCM_ATUAL
               INTO :GFCTB0A1.DPROCM-ATUAL
               FROM  DB2PRD.PARM_DATA_PROCM
              WHERE CSIST_PRINC = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0A1'      TO WRK-NOME-TAB
               MOVE 'GFCT0197'         TO GFCT0M-PROGRAMA
                                          GFCT0M-TRANSACAO
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE '0080'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-S9-3
               MOVE WRK-3-9            TO GFCTJO-COD-SQL-ERRO
               MOVE 9                  TO GFCTJO-ERRO
               MOVE SPACES             TO GFCTJO-DESC-MSG-ERRO
               MOVE 0010               TO GFCTJO-COD-MSG-ERRO
               PERFORM 2111-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRE O CURSOR 2                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2220-ABRIR-CSR02-GFCTB0J5       SECTION.
      *----------------------------------------------------------------*

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DINIC-VGCIA-REG  OF GFCTB0J5
                                          DFIM-VGCIA-REG   OF GFCTB0J5.
           EXEC SQL
               OPEN CSR02-GFCTB0J5
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN                 EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0J5'      TO WRK-NOME-TAB
               MOVE 'GFCT0197'         TO GFCT0M-PROGRAMA
                                          GFCT0M-TRANSACAO
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TCTA_AGPTO_PERSO' TO GFCT0M-NOME-TAB
               MOVE 'OPEN  '           TO GFCT0M-COMANDO-SQL
               MOVE '0090'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-S9-3
               MOVE WRK-3-9            TO GFCTJO-COD-SQL-ERRO
               MOVE 9                  TO GFCTJO-ERRO
               MOVE SPACES             TO GFCTJO-DESC-MSG-ERRO
               MOVE 0005               TO GFCTJO-COD-MSG-ERRO
               PERFORM 2111-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
PEROT ******************************************************************
  "   *ABRE O CURSOR 4                                                 *
  "   ******************************************************************
  "   *----------------------------------------------------------------*
  "    2221-ABRIR-CSR04-GFCTB0J5       SECTION.
  "   *----------------------------------------------------------------*
  "
  "        MOVE DPROCM-ATUAL           OF GFCTB0A1
  "                                    TO DINIC-VGCIA-REG  OF GFCTB0J5
  "                                       DFIM-VGCIA-REG   OF GFCTB0J5.
  "        EXEC SQL
  "            OPEN CSR04-GFCTB0J5
  "        END-EXEC.
  "
  "        IF (SQLCODE                 NOT EQUAL ZEROS) OR
  "           (SQLWARN                 EQUAL 'W')
  "            MOVE SPACES             TO GFCT0M-ERRO-SQL
  "            INITIALIZE              GFCT0M-ERRO-SQL
  "            MOVE '0005'             TO GFCTG2-COD-MSG
  "            PERFORM 1310-OBTER-DESC-MSG
  "            MOVE ' - GFCTB0J5'      TO WRK-NOME-TAB
  "            MOVE 'GFCT0197'         TO GFCT0M-PROGRAMA
  "                                       GFCT0M-TRANSACAO
  "            MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
  "            MOVE 'TCTA_AGPTO_PERSO' TO GFCT0M-NOME-TAB
  "            MOVE 'OPEN  '           TO GFCT0M-COMANDO-SQL
  "            MOVE '0100'             TO GFCT0M-LOCAL
  "            MOVE SQLCA              TO GFCT0M-SQLCA-AREA
  "            MOVE SQLCODE            TO GFCT0M-SQLCODE
  "                                       WRK-S9-3
  "            MOVE WRK-3-9            TO GFCTJO-COD-SQL-ERRO
  "            MOVE 9                  TO GFCTJO-ERRO
  "            MOVE SPACES             TO GFCTJO-DESC-MSG-ERRO
  "            MOVE 0005               TO GFCTJO-COD-MSG-ERRO
  "            PERFORM 2111-CONCATENAR-MENSAGEM
  "            PERFORM 3000-FINALIZAR
  "        END-IF.
  "
  "   *----------------------------------------------------------------*
  "    2221-99-FIM.                    EXIT.
PEROT *----------------------------------------------------------------*

      ******************************************************************
      *EFETUA LEITURA PELO CURSOR 2                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2230-LER-CSR02-GFCTB0J5         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR02-GFCTB0J5 INTO
                   :GFCTB0J5.CAG-BCRIA,
                   :GFCTB0J5.CCTA-BCRIA-CLI,
VI0618             :GFCTB0J5.DINIC-VGCIA-REG,
VI0618             :GFCTB0J5.DFIM-VGCIA-REG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZERO AND +100) OR
              (SQLWARN                 EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0J5'      TO WRK-NOME-TAB
               MOVE 'GFCT0197'         TO GFCT0M-PROGRAMA
                                          GFCT0M-TRANSACAO
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TCTA_AGPTO_PERSO' TO GFCT0M-NOME-TAB
               MOVE 'FETCH  '          TO GFCT0M-COMANDO-SQL
               MOVE '0110'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-S9-3
               MOVE WRK-3-9            TO GFCTJO-COD-SQL-ERRO
               MOVE 9                  TO GFCTJO-ERRO
               MOVE SPACES             TO GFCTJO-DESC-MSG-ERRO
               MOVE 0006               TO GFCTJO-COD-MSG-ERRO
               PERFORM 2111-CONCATENAR-MENSAGEM
               PERFORM 2250-FECHAR-CSR02-GFCTB0J5
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTJO-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO GFCTJO-ERRO
                   MOVE ZERO           TO GFCTJO-COD-SQL-ERRO
                   MOVE 0010           TO GFCTJO-COD-MSG-ERRO
                   MOVE '0010'         TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
BI1211             PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
PEROT ******************************************************************
  "   *EFETUA LEITURA PELO CURSOR 4                                    *
  "   ******************************************************************
  "   *----------------------------------------------------------------*
  "    2231-LER-CSR04-GFCTB0J5         SECTION.
  "   *----------------------------------------------------------------*
  "
  "        EXEC SQL
  "          FETCH CSR04-GFCTB0J5 INTO
  "                :GFCTB0J5.CAG-BCRIA,
  "                :GFCTB0J5.CCTA-BCRIA-CLI,
VI0618             :GFCTB0J5.DINIC-VGCIA-REG,
VI0618             :GFCTB0J5.DFIM-VGCIA-REG
  "        END-EXEC.
  "
  "        IF (SQLCODE                 NOT EQUAL ZERO AND +100) OR
  "           (SQLWARN                 EQUAL 'W')
  "            MOVE SPACES             TO GFCT0M-ERRO-SQL
  "            INITIALIZE              GFCT0M-ERRO-SQL
  "            MOVE '0006'             TO GFCTG2-COD-MSG
  "            PERFORM 1310-OBTER-DESC-MSG
  "            MOVE ' - GFCTB0J5'      TO WRK-NOME-TAB
  "            MOVE 'GFCT0197'         TO GFCT0M-PROGRAMA
  "                                       GFCT0M-TRANSACAO
  "            MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
  "            MOVE 'TCTA_AGPTO_PERSO' TO GFCT0M-NOME-TAB
  "            MOVE 'FETCH  '          TO GFCT0M-COMANDO-SQL
  "            MOVE '0120'             TO GFCT0M-LOCAL
  "            MOVE SQLCA              TO GFCT0M-SQLCA-AREA
  "            MOVE SQLCODE            TO GFCT0M-SQLCODE
  "                                       WRK-S9-3
  "            MOVE WRK-3-9            TO GFCTJO-COD-SQL-ERRO
  "            MOVE 9                  TO GFCTJO-ERRO
  "            MOVE SPACES             TO GFCTJO-DESC-MSG-ERRO
  "            MOVE 0006               TO GFCTJO-COD-MSG-ERRO
  "            PERFORM 2111-CONCATENAR-MENSAGEM
  "            PERFORM 2250-FECHAR-CSR02-GFCTB0J5
  "            PERFORM 3000-FINALIZAR
  "        END-IF.
  "
  "        IF  SQLCODE                 EQUAL +100
  "            MOVE 'S'                TO WRK-FINALIZAR
  "                                       GFCTJO-FIM
  "            IF  IND-1               EQUAL ZEROS
  "                MOVE 1              TO GFCTJO-ERRO
  "                MOVE ZERO           TO GFCTJO-COD-SQL-ERRO
  "                MOVE 0010           TO GFCTJO-COD-MSG-ERRO
  "                MOVE '0010'         TO GFCTG2-COD-MSG
  "                PERFORM 1310-OBTER-DESC-MSG
  "                PERFORM 3000-FINALIZAR
  "            END-IF
  "        END-IF.
  "
  "   *----------------------------------------------------------------*
  "    2231-99-FIM.                    EXIT.
PEROT *----------------------------------------------------------------*

      ******************************************************************
      *  MONTA AS OCORRENCIAS NA AREA DE SAIDA                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2240-PROCESSAR-SEL              SECTION.
      *----------------------------------------------------------------*

           MOVE CAG-BCRIA              OF GFCTB0J5
                                       TO GFCTJO-AGENCIA  (IND-1).
           MOVE CCTA-BCRIA-CLI         OF GFCTB0J5
                                       TO GFCTJO-CONTA    (IND-1).
           MOVE DINIC-VGCIA-REG        OF GFCTB0J5
                                       TO GFCTJO-DATA-INIC(IND-1).

VI0618     MOVE DFIM-VGCIA-REG         OF GFCTB0J5
VI0618                                 TO GFCTJO-DATA-FIM (IND-1).

           PERFORM 2131-PESQUISAR-NM-CLIENTE.

           MOVE GFCTJO-QTDE-TOT-REG    TO WRK-GFCTJO-QTDE-TOT-REG.
BI1211     MOVE GFCTJO-QTDE-OCOR       TO WRK-QTDE-OCOR.

           ADD 1                       TO WRK-GFCTJO-QTDE-TOT-REG
                                          WRK-QTDE-OCOR.

           MOVE WRK-GFCTJO-QTDE-TOT-REG
                                       TO GFCTJO-QTDE-TOT-REG.
           MOVE WRK-QTDE-OCOR          TO GFCTJO-QTDE-OCOR.

PEROT      IF GFCTJN-AGENCIA-FIL     EQUAL ZEROS
              PERFORM 2230-LER-CSR02-GFCTB0J5
PEROT      ELSE
  "           PERFORM 2231-LER-CSR04-GFCTB0J5
PEROT      END-IF.

      *----------------------------------------------------------------*
       2240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHA O CURSOR 2                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2250-FECHAR-CSR02-GFCTB0J5      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR02-GFCTB0J5
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0J5'      TO WRK-NOME-TAB
               MOVE 'GFCT0197'         TO GFCT0M-PROGRAMA
                                          GFCT0M-TRANSACAO
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TCTA_AGPTO_PERSO' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE   '         TO GFCT0M-COMANDO-SQL
               MOVE '0130'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-S9-3
               MOVE WRK-3-9            TO GFCTJO-COD-SQL-ERRO
               MOVE 9                  TO GFCTJO-ERRO
               MOVE SPACES             TO GFCTJO-DESC-MSG-ERRO
               MOVE 0011               TO GFCTJO-COD-MSG-ERRO
               PERFORM 2111-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      ******************************************************************
      *FECHA O CURSOR 4                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2251-FECHAR-CSR04-GFCTB0J5      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR04-GFCTB0J5
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE ' - GFCTB0J5'      TO WRK-NOME-TAB
               MOVE 'GFCT0197'         TO GFCT0M-PROGRAMA
                                          GFCT0M-TRANSACAO
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TCTA_AGPTO_PERSO' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE   '         TO GFCT0M-COMANDO-SQL
               MOVE '0140'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-S9-3
               MOVE WRK-3-9            TO GFCTJO-COD-SQL-ERRO
               MOVE 9                  TO GFCTJO-ERRO
               MOVE SPACES             TO GFCTJO-DESC-MSG-ERRO
               MOVE 0011               TO GFCTJO-COD-MSG-ERRO
               PERFORM 2111-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2251-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FINALIZA O PROCESSAMENTO E RETORNA AO PROGRAMA CHAMADOR.        *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

