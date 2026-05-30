      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0020.
       AUTHOR.     JEAN CARLOS GAZZONI.
      *================================================================*
      *                   C P M  -  S I S T E M A S                    *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0020                                    *
      *    PROGRAMADOR.:   JEAN CARLOS GAZZONI     - CPM PATO BRANCO   *
      *    ANALISTA CPM:   FRANCISMARA PAGNONCELLI  - CPM PATO BRANCO  *
      *    ANALISTA....:   MARCELO T. C. BARROS    - PWI / GRUPO 67    *
      *    DATA........:   22/02/2005                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GRUPO DE LANCAMENTOS -  EXCLUSAO LOGICA.    *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                         INCLUDE/BOOK    *
      *                    DB2PRD.GRP_COD_LCTO           GFCTB062      *
      *                    DB2PRD.COMPS_GRP_SERVC        GFCTB027      *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#GFCT5A - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCT5B - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS                      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5000 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                  *
      *================================================================*
           EJECT
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

           EJECT
      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** GFCT0020 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA INDEXADORES ***'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(001)         VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA CAMPOS DO MODULO GFCT5000 ***'.
      *----------------------------------------------------------------*

       01  WRK-5000-AREA.
           03  WRK-5000-AREA-ENVIA.
               05  WRK-5000-AMBIENTE     PIC X(001)        VALUE SPACES.
               05  WRK-5000-COD-MSG      PIC 9(004)        VALUE ZEROS.
           03  WRK-5000-AREA-RECEBE.
               05  WRK-5000-COD-RETORNO  PIC 9(002)        VALUE ZEROS.
               05  WRK-5000-COD-SQL-ERRO PIC 9(003)        VALUE ZEROS.
               05  WRK-5000-DESC-MSG     PIC X(070)        VALUE SPACES.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EJECT
           EXEC SQL
               INCLUDE GFCTB062
           END-EXEC.

           EJECT
           EXEC SQL
               INCLUDE GFCTB027
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** GFCT0020 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

-INC 'I#GFCT5A'

           EJECT

-INC 'I#GFCT5B'

           EJECT

-INC I#GFCT0M

           EJECT
      *================================================================*
       PROCEDURE DIVISION              USING GFCT5A-ENTRADA
                                             GFCT5B-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
       000000-INICIAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 100000-INICIALIZAR.

           PERFORM 200000-EXCLUIR-LOGICA
             VARYING IND-1             FROM 1 BY 1
               UNTIL IND-1             GREATER GFCT5A-QTDE-OCOR.

           PERFORM  TEST AFTER VARYING IND-1 FROM 1 BY 1
            UNTIL  GFCT5B-DESC-MSG-ERRO-OCOR(IND-1)NOT EQUAL SPACES OR
                   IND-1             GREATER GFCT5A-QTDE-OCOR

                   MOVE  GFCT5B-DESC-MSG-ERRO-OCOR(IND-1)
                                              TO GFCT5B-DESC-MSG-ERRO
                   MOVE  1                    TO GFCT5B-ERRO

           END-PERFORM.

           PERFORM 300000-FINALIZAR.

      *----------------------------------------------------------------*
       000000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          GFCT5B-SAIDA.

           INITIALIZE GFCT0M-AREA-ERROS
                      GFCT5B-SAIDA.

           PERFORM 110000-MONTAR-AREA-SAIDA.

           PERFORM 120000-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       110000-MONTAR-AREA-SAIDA        SECTION.
      *----------------------------------------------------------------*

           MOVE  205                   TO GFCT5B-LL.
           MOVE  GFCT5A-TRANSACAO      TO GFCT5B-TRANSACAO.
           MOVE  GFCT5A-FUNCAO         TO GFCT5B-FUNCAO.
           MOVE  GFCT5A-TERMINAL       TO GFCT5B-TERMINAL.
           MOVE  GFCT5A-FUNC-BDSCO     TO GFCT5B-FUNC-BDSCO.
           MOVE  GFCT5A-QTDE-OCOR      TO GFCT5B-QTDE-OCOR.
           MOVE  ZEROS                 TO GFCT5B-ERRO
                                          GFCT5B-COD-SQL-ERRO
                                          GFCT5B-COD-MSG-ERRO
                                          WRK-5000-COD-MSG.

           PERFORM 111000-OBTER-DESCRICAO-MSG.

           MOVE  WRK-5000-DESC-MSG     TO GFCT5B-DESC-MSG-ERRO.

           MOVE 'S'                    TO GFCT5B-FIM.

      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       111000-OBTER-DESCRICAO-MSG      SECTION.
      *----------------------------------------------------------------*

           MOVE 'O'                         TO WRK-5000-AMBIENTE.
           MOVE 'GFCT5000'                  TO WRK-MODULO.

           CALL 'POOL0081'                  USING WRK-MODULO
                                                  WRK-5000-AREA-ENVIA
                                                  WRK-5000-AREA-RECEBE.

           IF  WRK-5000-COD-RETORNO         NOT EQUAL ZEROS
               IF  WRK-5000-COD-RETORNO     EQUAL 01
                   MOVE  9                  TO GFCT5B-ERRO
                   MOVE  ZEROS              TO GFCT5B-COD-SQL-ERRO
                                               GFCT5B-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG   TO GFCT5B-DESC-MSG-ERRO
               ELSE
                   IF  WRK-5000-COD-RETORNO EQUAL 99
                       MOVE  9              TO GFCT5B-ERRO
                       MOVE  WRK-5000-COD-SQL-ERRO
                                            TO GFCT5B-COD-SQL-ERRO
                                               GFCT5B-COD-MSG-ERRO
                       MOVE WRK-5000-DESC-MSG
                                            TO GFCT5B-DESC-MSG-ERRO
                   END-IF
               END-IF
               MOVE SPACES                  TO GFCT5B-SAIDA
               INITIALIZE                      GFCT5B-SAIDA
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       111000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       120000-CONSISTIR-DADOS          SECTION.
      *----------------------------------------------------------------*

           IF  GFCT5B-TRANSACAO        EQUAL SPACES     OR
               GFCT5B-TRANSACAO        EQUAL LOW-VALUES OR
               GFCT5B-FUNCAO           EQUAL SPACES     OR
               GFCT5B-FUNCAO           EQUAL LOW-VALUES OR
               GFCT5B-QTDE-OCOR        NOT NUMERIC      OR
               GFCT5B-QTDE-OCOR        EQUAL ZEROS      OR
               GFCT5B-QTDE-OCOR        GREATER 5        OR
               GFCT5B-FUNC-BDSCO       EQUAL LOW-VALUES OR
               GFCT5B-FUNC-BDSCO       EQUAL SPACES
               MOVE  1                 TO GFCT5B-ERRO
                                          GFCT5B-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE  ZEROS             TO GFCT5B-COD-SQL-ERRO
               PERFORM 111000-OBTER-DESCRICAO-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCT5B-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       120000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       200000-EXCLUIR-LOGICA           SECTION.
      *----------------------------------------------------------------*

           PERFORM 210000-SELECIONAR-GFCTB027.

           IF  GFCT5B-ERRO-OCOR(IND-1) EQUAL ZEROS
               PERFORM 220000-ATUALIZAR-GFCTB062
           END-IF.

           ADD 115                     TO GFCT5B-LL.

      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       210000-SELECIONAR-GFCTB027      SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT5A-COD-GRP(IND-1)  TO CGRP-LCTO OF GFCTB027.

           EXEC SQL
             SELECT    CGRP_LCTO
             INTO     :GFCTB027.CGRP-LCTO
             FROM      DB2PRD.COMPS_GRP_LCTO
             WHERE     CGRP_LCTO = :GFCTB027.CGRP-LCTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -811    AND
                                                 +100)             OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE                 GFCT0M-ERRO-SQL
               MOVE 'GFCT0020'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'COMPS_GRP_LCTO '  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE  '0010'            TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT5B-ERRO
               MOVE  SQLCODE           TO GFCT5B-COD-SQL-ERRO
               MOVE  0012              TO GFCT5B-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB027'
               DELIMITED BY '  '       INTO GFCT5B-DESC-MSG-ERRO
               MOVE SPACES             TO GFCT5B-DADOS
               INITIALIZE                 GFCT5B-DADOS
               PERFORM 300000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS OR -811
               MOVE 0103               TO WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               MOVE WRK-5000-DESC-MSG  TO
                                       GFCT5B-DESC-MSG-ERRO-OCOR(IND-1)
               MOVE 01                 TO GFCT5B-ERRO-OCOR(IND-1)
           END-IF.

      *----------------------------------------------------------------*
       210000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       220000-ATUALIZAR-GFCTB062       SECTION.
      *----------------------------------------------------------------*

           MOVE CGRP-LCTO OF GFCTB027  TO CGRP-LCTO OF GFCTB062.
           MOVE 1                      TO CINDCD-EXCL-LOGCA OF GFCTB062.

           EXEC SQL
             UPDATE    DB2PRD.GRP_COD_LCTO
               SET     CINDCD_EXCL_LOGCA  = :GFCTB062.CINDCD-EXCL-LOGCA
             WHERE     CGRP_LCTO          = :GFCTB062.CGRP-LCTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)  OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE                 GFCT0M-ERRO-SQL
               MOVE 'GFCT0020'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GRP_COD_LCTO'     TO GFCT0M-NOME-TAB
               MOVE 'UPDATE'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE  '0020'            TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT5B-ERRO
               MOVE  SQLCODE           TO GFCT5B-COD-SQL-ERRO
               MOVE  0019              TO GFCT5B-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB062'
               DELIMITED BY '  '       INTO GFCT5B-DESC-MSG-ERRO
               MOVE SPACES             TO GFCT5B-DADOS
               INITIALIZE                 GFCT5B-DADOS
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       220000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       300000-FINALIZAR                SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
