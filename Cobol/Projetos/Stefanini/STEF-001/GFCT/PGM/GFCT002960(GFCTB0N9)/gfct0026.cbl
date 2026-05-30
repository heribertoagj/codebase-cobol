      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0026.
       AUTHOR.     DAIANE APARECIDA FIORIO.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0026                                    *
      *    PROGRAMADORA:   DAIANE APARECIDA FIORIO - CPM PATO BRANCO   *
      *    ANALISTA CPM:   MARCIO CRISTIANO CUSTIN - CPM PATO BRANCO   *
      *    ANALISTA....:   MARCELO BARROS          - PROCKWORK / GP.67 *
      *    DATA........:   23/02/2005                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   EFETUAR CONSULTA DE GRUPOS DE LANCAMENTOS.  *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                         INCLUDE/BOOK    *
      *                      DB2PRD.COMPS_GRP_LCTO       GFCTB027      *
      *                      DB2PRD.CATLG_LCTO_MUBCO     CDLCB003      *
      *                      DB2PRD.PERIF_SBIN           SBINB026      *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#GFCT5C - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCT5D - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS                      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5000 - OBTER DESCRICAO DE MENSAGENS                     *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                   *
      *================================================================*
080130*----------------------------------------------------------------*
080130*           SONDA - PROCWORK CONSULTORIA - ALTERACAO             *
080130*----------------------------------------------------------------*
080130*    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA      / GRUPO 50 *
080130*    DATA........:   30/01/2008                                  *
080130*    OBJETIVO....:   SERA RETIRADO TODA CONSISTENCIA AO CAMPO    *
080130*                    PERIFERICO.                                 *
080130*----------------------------------------------------------------*
           EJECT
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
           '*** GFCT0026 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** INDEXADOR ***'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(003)         VALUE ZEROS.
       77  IND-2                       PIC  9(005)         VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-NOME-TAB                PIC  X(011)         VALUE SPACES.
       77  WRK-FINALIZAR               PIC  X(001)         VALUE SPACES.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA O MODULO GFCT5000 ***'.
      *----------------------------------------------------------------*

       01  WRK-5000-AREA.
           03  WRK-5000-AREA-ENVIA.
               05  WRK-5000-AMBIENTE     PIC  X(001)       VALUE SPACES.
               05  WRK-5000-COD-MSG      PIC  9(004)       VALUE ZEROS.
           03  WRK-5000-AREA-RECEBE.
               05  WRK-5000-COD-RETORNO  PIC  9(002)       VALUE ZEROS.
               05  WRK-5000-COD-SQL-ERRO PIC  9(003)       VALUE ZEROS.
               05  WRK-5000-DESC-MSG     PIC  X(070)       VALUE SPACES.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.
           EJECT

           EXEC SQL
             INCLUDE GFCTB027
           END-EXEC.
           EJECT

           EXEC SQL
             INCLUDE 'CDLCB003'
           END-EXEC.
           EJECT

           EXEC SQL
             INCLUDE SBINB026
           END-EXEC.
           EJECT

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA DECLARACAO DO CURSOR ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               DECLARE CRS-GFCTB027    CURSOR FOR
               SELECT  CGRP_LCTO,
                       CIDTFD_LCTO,
                       CIDTFD_SNAL_LCTO,
080130*                CTPO_PERIF,
                       DINIC_GRP_LCTO,
                       DFIM_GRP_LCTO
               FROM    DB2PRD.COMPS_GRP_LCTO
               WHERE   CGRP_LCTO       = :GFCTB027.CGRP-LCTO
               ORDER BY DINIC_GRP_LCTO DESC,
                        CIDTFD_LCTO,
080130*                 CTPO_PERIF,
                        CIDTFD_SNAL_LCTO
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** GFCT0026 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

-INC 'I#GFCT5C'
-INC 'I#GFCT5D'
-INC I#GFCT0M

           EJECT
      *================================================================*
       PROCEDURE DIVISION              USING GFCT5C-ENTRADA
                                             GFCT5D-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
       000000-ROTINA-PRINCIPAL         SECTION.
      *----------------------------------------------------------------*

           PERFORM 100000-INICIALIZAR.

           PERFORM 200000-PROCESSAR.

           PERFORM 300000-FINALIZAR.

      *----------------------------------------------------------------*
       000000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.
      *----------------------------------------------------------------*

           PERFORM 110000-MONTAR-AREA-SAIDA.

           PERFORM 120000-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       110000-MONTAR-AREA-SAIDA        SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  SQLCA
                                       GFCT5D-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 195                    TO GFCT5D-LL.
           MOVE GFCT5C-TRANSACAO       TO GFCT5D-TRANSACAO.
           MOVE GFCT5C-FUNCAO          TO GFCT5D-FUNCAO.
           MOVE GFCT5C-PONTEIRO        TO GFCT5D-PONTEIRO.
           MOVE GFCT5C-FUNC-BDSCO      TO GFCT5D-FUNC-BDSCO.
           MOVE GFCT5C-TERMINAL        TO GFCT5D-TERMINAL.
           MOVE GFCT5C-GRUPO-LANCTO    TO GFCT5D-GRUPO-LANCTO.
           MOVE 'N'                    TO GFCT5D-FIM
                                          WRK-FINALIZAR.
           MOVE ZEROS                  TO GFCT5D-ERRO
                                          GFCT5D-COD-SQL-ERRO
                                          GFCT5D-COD-MSG-ERRO
                                          WRK-5000-COD-MSG.

           IF  GFCT5C-QTDE-OCOR        NOT NUMERIC
               MOVE ZEROS              TO GFCT5D-QTDE-OCOR
           ELSE
               MOVE GFCT5C-QTDE-OCOR   TO GFCT5D-QTDE-OCOR
           END-IF.

           IF  GFCT5C-QTDE-TOT-REG     NOT NUMERIC
               MOVE ZEROS              TO GFCT5D-QTDE-TOT-REG
           ELSE
               MOVE GFCT5C-QTDE-TOT-REG TO GFCT5D-QTDE-TOT-REG
           END-IF.

           PERFORM 111000-OBTER-DESC-MSG.

           MOVE WRK-5000-DESC-MSG      TO GFCT5D-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       111000-OBTER-DESC-MSG           SECTION.
      *----------------------------------------------------------------*

           MOVE 'O'                    TO WRK-5000-AMBIENTE.
           MOVE 'GFCT5000'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5000-AREA-ENVIA
                                             WRK-5000-AREA-RECEBE.

           IF  WRK-5000-COD-RETORNO           NOT EQUAL ZEROS
               IF  WRK-5000-COD-RETORNO       EQUAL 01
                   MOVE 9                     TO GFCT5D-ERRO
                   MOVE ZEROS                 TO GFCT5D-COD-SQL-ERRO
                                                 GFCT5D-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG     TO GFCT5D-DESC-MSG-ERRO
               ELSE
                   IF  WRK-5000-COD-RETORNO   EQUAL 99
                       MOVE 9                 TO GFCT5D-ERRO
                       MOVE WRK-5000-COD-SQL-ERRO
                                              TO GFCT5D-COD-SQL-ERRO
                       MOVE ZEROS             TO GFCT5D-COD-MSG-ERRO
                       MOVE WRK-5000-DESC-MSG TO GFCT5D-DESC-MSG-ERRO
                   END-IF
               END-IF
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       111000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       120000-CONSISTIR-DADOS          SECTION.
      *----------------------------------------------------------------*

           IF (GFCT5C-TRANSACAO        EQUAL SPACES        OR
                                       EQUAL LOW-VALUES)   OR
              (GFCT5C-FUNCAO           EQUAL SPACES        OR
                                       EQUAL LOW-VALUES)   OR
              (GFCT5C-PONTEIRO         NOT NUMERIC)
               MOVE 1                  TO GFCT5D-ERRO
                                          GFCT5D-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE ZEROS              TO GFCT5D-COD-SQL-ERRO
               MOVE 'S'                TO GFCT5D-FIM
               PERFORM 111000-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCT5D-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       120000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       200000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 210000-ABRIR-CURSOR.

           PERFORM 220000-LER-CURSOR.
********   DISPLAY 'GFCT5C-PONTEIRO ' GFCT5C-PONTEIRO
           IF  GFCT5C-PONTEIRO                 GREATER ZEROS
               PERFORM 220000-LER-CURSOR       VARYING
                       IND-2   FROM 1 BY 1     UNTIL
                       IND-2   GREATER         GFCT5C-PONTEIRO OR
                       WRK-FINALIZAR           EQUAL 'S'
           END-IF.

           PERFORM 230000-MONTAR-OCORRENCIAS   VARYING
                   IND-2   FROM 1 BY 1         UNTIL
                   IND-2   GREATER 5           OR
                   WRK-FINALIZAR               EQUAL 'S'.

           PERFORM 240000-FECHAR-CURSOR.

      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       210000-ABRIR-CURSOR             SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT5C-GRUPO-LANCTO    TO CGRP-LCTO        OF GFCTB027.

           EXEC SQL
               OPEN CRS-GFCTB027
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'GFCT0026'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'COMPS_GRP_LCTO  ' TO GFCT0M-NOME-TAB
               MOVE 'OPEN    '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCT5D-COD-SQL-ERRO
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT5D-ERRO
               MOVE  0005              TO GFCT5D-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESC-MSG
               MOVE ' - GFCTB027'       TO WRK-NOME-TAB
               PERFORM 211000-OBTER-MSG-DB2
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       210000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       211000-OBTER-MSG-DB2            SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING IND-1                     FROM
                   LENGTH OF WRK-5000-DESC-MSG BY -1 UNTIL
                   WRK-5000-DESC-MSG(IND-1:1)        NOT EQUAL SPACES OR
                   IND-1                             EQUAL ZEROS
           END-PERFORM.

           MOVE WRK-NOME-TAB         TO WRK-5000-DESC-MSG
                                     (IND-1 + 1:LENGTH OF WRK-NOME-TAB).
           MOVE WRK-5000-DESC-MSG    TO GFCT5D-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       211000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       220000-LER-CURSOR               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH   CRS-GFCTB027    INTO
                      :GFCTB027.CGRP-LCTO,
                      :GFCTB027.CIDTFD-LCTO,
                      :GFCTB027.CIDTFD-SNAL-LCTO,
080130*               :GFCTB027.CTPO-PERIF,
                      :GFCTB027.DINIC-GRP-LCTO,
                      :GFCTB027.DFIM-GRP-LCTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'GFCT0026'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'COMPS_GRP_LCOT  ' TO GFCT0M-NOME-TAB
               MOVE 'FECTH   '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCT5D-COD-SQL-ERRO
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT5D-ERRO
               MOVE  0006              TO GFCT5D-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESC-MSG
               MOVE ' - GFCTB027'       TO WRK-NOME-TAB
               PERFORM 211000-OBTER-MSG-DB2
               PERFORM 300000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCT5D-FIM
               IF  IND-2               EQUAL ZEROS
*****************  MOVE ZEROS          TO GFCT5D-ERRO
                   MOVE 1              TO GFCT5D-ERRO
                                          GFCT5D-COD-SQL-ERRO
                   MOVE 0370           TO GFCT5D-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 111000-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG TO GFCT5D-DESC-MSG-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       220000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       230000-MONTAR-OCORRENCIAS       SECTION.
      *----------------------------------------------------------------*

           COMPUTE GFCT5D-LL           = (GFCT5D-LL + 163).

           PERFORM 231000-OBTER-DESC-LCTO.

           PERFORM 232000-OBTER-DESC-PERIF.

           MOVE CIDTFD-LCTO            OF GFCTB027
                                       TO GFCT5D-COD-LANCTO(IND-2).
080130     MOVE ZEROS                  TO GFCT5D-COD-PERIF(IND-2).
           MOVE CIDTFD-SNAL-LCTO       OF GFCTB027
                                       TO GFCT5D-DEB-CRED-AMB(IND-2).
           MOVE DINIC-GRP-LCTO         OF GFCTB027
                                       TO GFCT5D-DINI-VIG(IND-2).
           MOVE DFIM-GRP-LCTO          OF GFCTB027
                                       TO GFCT5D-DFIM-VIG(IND-2).

           ADD 1                       TO GFCT5D-QTDE-TOT-REG
                                          GFCT5D-PONTEIRO.

           PERFORM 220000-LER-CURSOR.

      *----------------------------------------------------------------*
       230000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       231000-OBTER-DESC-LCTO          SECTION.
      *----------------------------------------------------------------*

           MOVE CIDTFD-LCTO            OF GFCTB027
                                       TO CIDTFD-LCTO      OF CDLCB003.
           MOVE 237                    TO CBCO             OF CDLCB003.
CPM********MOVE CIDTFD-SNAL-LCTO       OF GFCTB027
CPM********                            TO CIDTFD-SNAL-LCTO OF CDLCB003.

           EXEC SQL
               SELECT  IIDTFD_HIST_LCTO
               INTO   :CDLCB003.IIDTFD-HIST-LCTO
               FROM    DB2PRD.CATLG_LCTO_MUBCO
               WHERE   CBCO                = :CDLCB003.CBCO
               AND     CIDTFD_LCTO         = :CDLCB003.CIDTFD-LCTO
CPM************AND     CIDTFD_SNAL_LCTO    = :CDLCB003.CIDTFD-SNAL-LCTO
           END-EXEC.

           IF (SQLCODE          NOT EQUAL ZEROS AND -811) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'GFCT0026'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'CATLG_LCTO_MUBCO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCT5D-COD-SQL-ERRO
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT5D-ERRO
               MOVE  0012              TO GFCT5D-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESC-MSG
               MOVE ' - CDLCB003'       TO WRK-NOME-TAB
               PERFORM 211000-OBTER-MSG-DB2
               PERFORM 300000-FINALIZAR
           END-IF.

***********IF  SQLCODE                 EQUAL +100
***********    PERFORM 231100-SELECT-CDLCB003
***********ELSE
               MOVE IIDTFD-HIST-LCTO   OF CDLCB003
                                       TO GFCT5D-DESC-LANCTO(IND-2).
***********END-IF.

      *----------------------------------------------------------------*
       231000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       231100-SELECT-CDLCB003          SECTION.
      *----------------------------------------------------------------*

           MOVE CIDTFD-LCTO            OF GFCTB027
                                       TO CIDTFD-LCTO      OF CDLCB003.
           MOVE 237                    TO CBCO             OF CDLCB003.
           MOVE ZEROS                  TO CIDTFD-SNAL-LCTO OF CDLCB003.

           EXEC SQL
               SELECT  IIDTFD_HIST_LCTO
               INTO   :CDLCB003.IIDTFD-HIST-LCTO
               FROM    DB2PRD.CATLG_LCTO_MUBCO
               WHERE   CBCO                = :CDLCB003.CBCO
               AND     CIDTFD_LCTO         = :CDLCB003.CIDTFD-LCTO
               AND     CIDTFD_SNAL_LCTO    = :CDLCB003.CIDTFD-SNAL-LCTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'GFCT0026'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'CATLG_LCTO_MUBCO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCT5D-COD-SQL-ERRO
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT5D-ERRO
               MOVE  0012              TO GFCT5D-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESC-MSG
               MOVE ' - CDLCB003'       TO WRK-NOME-TAB
080130         PERFORM 211000-OBTER-MSG-DB2
               PERFORM 300000-FINALIZAR
           END-IF.

           MOVE IIDTFD-HIST-LCTO       OF CDLCB003
                                       TO GFCT5D-DESC-LANCTO(IND-2).

      *----------------------------------------------------------------*
       231100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
080130*----------------------------------------------------------------*
080130 232000-OBTER-DESC-PERIF         SECTION.
080130*----------------------------------------------------------------*
080130
080130*    MOVE CTPO-PERIF             OF GFCTB027
080130*                                TO CTPO-PERIF       OF SBINB026.
080130*
080130*    EXEC SQL
080130*        SELECT  ITPO_PERIF
080130*        INTO   :SBINB026.ITPO-PERIF
080130*        FROM    DB2PRD.PERIF_SBIN
080130*        WHERE   CTPO_PERIF      = :SBINB026.CTPO-PERIF
080130*    END-EXEC.
080130*
080130*    IF (SQLCODE                 NOT EQUAL ZEROS) OR
080130*       (SQLWARN0                EQUAL 'W')
080130*        MOVE 'GFCT0026'         TO GFCT0M-PROGRAMA
080130*        MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
080130*        MOVE 'PERIF_SBIN      ' TO GFCT0M-NOME-TAB
080130*        MOVE 'SELECT  '         TO GFCT0M-COMANDO-SQL
080130*        MOVE  SQLCODE           TO GFCT0M-SQLCODE
080130*                                   GFCT5D-COD-SQL-ERRO
080130*        MOVE '0050'             TO GFCT0M-LOCAL
080130*        MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
080130*        MOVE  9                 TO GFCT5D-ERRO
080130*        MOVE  0012              TO GFCT5D-COD-MSG-ERRO
080130*                                   WRK-5000-COD-MSG
080130*        PERFORM 111000-OBTER-DESC-MSG
080130*        MOVE ' - SBINB026'       TO WRK-NOME-TAB
080130*        PERFORM 211000-OBTER-MSG-DB2
080130*        PERFORM 300000-FINALIZAR
080130*    END-IF.
080130*
080130*    MOVE ITPO-PERIF             OF SBINB026
080130*                                TO GFCT5D-DESC-PERIF(IND-2).
080130*
080130     MOVE SPACES                 TO GFCT5D-DESC-PERIF(IND-2).
080130
080130
080130
080130*----------------------------------------------------------------*
080130 232000-99-FIM.                  EXIT.
080130*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       240000-FECHAR-CURSOR            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE   CRS-GFCTB027
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'GFCT0026'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'COMPS_GRP_LCTO  ' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE   '         TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCT5D-COD-SQL-ERRO
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCT5D-ERRO
               MOVE  0011              TO GFCT5D-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESC-MSG
               MOVE ' - GFCTB027'       TO WRK-NOME-TAB
               PERFORM 211000-OBTER-MSG-DB2
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       240000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       300000-FINALIZAR                SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

