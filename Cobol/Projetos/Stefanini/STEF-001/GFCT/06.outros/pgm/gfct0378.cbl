      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0378.
       AUTHOR. FERNANDO SCHUTZ.
      *================================================================*
      *                   C P M   S I S T E M A S                      *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT0378                                     *
      *    PROGRAMADOR  : FERNANDO SCHUTZ              - CPM/FPOLIS    *
      *    ANALISTA CPM : RODRIGO WILLAIM              - CPM/FPOLIS    *
      *    ANALISTA     : MARCELO  - PROCWORK          - GRUPO 50      *
      *    DATA         : 27/10/2005                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      CONSULTA OU EFETIVA DEFERIMENTO DE ESTORNO POR AGRUPA-    *
      *      MENTO.                                                    *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *     DB2                                                        *
      *      TABLE                                 INCLUDE/BOOK        *
      *      DB2PRD.PARM_DATA_PROCM                  GFCTB0A1          *
      *      DB2PRD.PDIDO_ESTRN_AG                   GFCTB0A3          *
      *      DB2PRD.PDIDO_ESTRN_CLI                  GFCTB0A4          *
      *      DB2PRD.PDIDO_ESTRN_EMPR                 GFCTB0A6          *
      *      DB2PRD.PDIDO_ESTRN_GRP                  GFCTB0A7          *
      *      DB2PRD.PDIDO_ESTRN_PAB                  GFCTB0A8          *
      *      DB2PRD.PDIDO_ESTRN_PSTAL                GFCTB0A9          *
      *      DB2PRD.PDIDO_ESTRN_SGMTO                GFCTB0B0          *
      *      DB2PRD.TPO_OPER_MOTVO                   GFCTB0G0          *
      *      DB2PRD.MOTVO_JUSTF_OPER                 GFCTB089          *
      *      DB2PRD.TTPO_SGMTO_GSTAO                 GFCTB0H5          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5000 - OBTER DESCRICAO DE MENSAGENS.                  *
      *      GFCT5500 - VERIFICAR SE O SISTEMA ESTA LIBERADO.          *
      *      GFCT5006 - OBTER DESCRICAO DA AGENCIA                     *
      *      GFCT5060 - OBTER NOME DO CLIENTE                          *
      *      GFCT5090 - OBTER DESCRICAO DO CORRESPONDENTE BANCARIO     *
      *      GFCT5109 - OBTER DESCRICAO DO PAB                         *
      *      GFCT5110 - OBTER NOME DO CLIENTE POR CPF/CNPJ             *
      *      POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                *
      *                                                                *
      *    NAVEGACAO:                                                  *
      *      CHAMADO POR: GFCT7385                                     *
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
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT0378  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*          AUXILIARES          *'.
      *----------------------------------------------------------------*

       01  WRK-MODULO                  PIC X(08)           VALUE SPACES.
       01  WRK-NOME-TAB                PIC X(08)           VALUE SPACES.
       01  WRK-CONTA                   PIC X(01)           VALUE SPACES.

       01  WRK-COD-AGENCIA             PIC 9(05)           VALUE ZEROS.

       01  WRK-MSG05.
           03  FILLER                  PIC X(26)           VALUE
           'ERRO NA CHAMADA DO MODULO '.
           03  WRK-MSG05-MODULO        PIC X(08)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    VARIAVEIS DE NULIDADE     *'.
      *----------------------------------------------------------------*

       01  WRK-GFCTB0B0-NULL.
           03  WRK-CSGMTO-GSTAO-TARIF-NULL
                                       PIC S9(04) COMP     VALUE +0.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA DE MODULO 5000     *'.
      *----------------------------------------------------------------*

       01  WRK-5000-AREA.
           03  WRK-5000-AREA-ENVIA.
               05  WRK-5000-AMBIENTE   PIC X(01)           VALUE SPACES.
               05  WRK-5000-COD-MSG    PIC 9(04)           VALUE ZEROS.
           03  WRK-5000-AREA-RECEBE.
               05  WRK-5000-COD-RETORNO
                                       PIC 9(02)           VALUE ZEROS.
               05  WRK-5000-COD-SQL-ERRO
                                       PIC 9(03)           VALUE ZEROS.
               05  WRK-5000-DESC-MSG   PIC X(70)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA DE MODULO 5500     *'.
      *----------------------------------------------------------------*

       01  WRK-5500-AREA-SAIDA.
           03  WRK-5500-COD-RETORNO    PIC 9(02)           VALUE ZEROS.
           03  WRK-5500-COD-SQL-ERRO   PIC 9(03)           VALUE ZEROS.
           03  WRK-5500-DESC-MSG       PIC X(70)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA DO MODULO GFCT5006   *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCT1V'.
       COPY 'I#GFCT1X'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA DO MODULO GFCT5060   *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTBR'.
       COPY 'I#GFCTBS'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA DO MODULO GFCT5090   *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTGB'.
       COPY 'I#GFCTGC'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA DO MODULO GFCT5109   *'.
      *----------------------------------------------------------------*

      *COPY I#GFCTGB.
      *COPY I#GFCTGC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA DO MODULO GFCT9110   *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTIU'.
       COPY 'I#GFCTIV'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA DE TABELAS DB2     *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A3
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A4
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A6
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A7
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A8
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A9
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0B0
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0F3
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0G0
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB089
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0H5
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    FIM DA WORKING GFCT0378   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTGN'.
       COPY 'I#GFCTGO'.
       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTGN-ENTRADA
                                                      GFCTGO-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
       000000-ROTINA-PRINCIPAL         SECTION.
      *----------------------------------------------------------------*

           PERFORM 100000-INICIALIZAR.

           PERFORM 200000-PROCESSAR-PROGRAMA.

           PERFORM 300000-FINALIZAR.

      *----------------------------------------------------------------*
       000000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.
      *----------------------------------------------------------------*

           PERFORM 110000-MONTAR-AREA-SAIDA.

           PERFORM 120000-VERIFICAR-ONLINE.

      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       110000-MONTAR-AREA-SAIDA        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGO-SAIDA.

           INITIALIZE                  GFCTGO-SAIDA.

           MOVE 1010                   TO GFCTGO-LL.
           MOVE GFCTGN-TRANSACAO       TO GFCTGO-TRANSACAO.
           MOVE GFCTGN-FUNC-BDSCO      TO GFCTGO-FUNC-BDSCO.
           MOVE GFCTGN-TERMINAL        TO GFCTGO-TERMINAL.
           MOVE GFCTGN-FUNCAO          TO GFCTGO-FUNCAO.
           MOVE ZEROS                  TO GFCTGO-ERRO
                                          GFCTGO-COD-SQL-ERRO
                                          GFCTGO-COD-MSG-ERRO
                                          WRK-5000-COD-MSG.

           PERFORM 111000-OBTER-DESC-MENSAGEM.

           MOVE WRK-5000-DESC-MSG      TO GFCTGO-DESC-MSG-ERRO.
           MOVE 'S'                    TO GFCTGO-FIM.

      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       111000-OBTER-DESC-MENSAGEM      SECTION.
      *----------------------------------------------------------------*

           MOVE 'O'                    TO WRK-5000-AMBIENTE.
           MOVE 'GFCT5000'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5000-AREA-ENVIA
                                             WRK-5000-AREA-RECEBE.

           IF  WRK-5000-COD-RETORNO    NOT EQUAL ZEROS
               IF  WRK-5000-COD-RETORNO
                                       EQUAL 01
                   MOVE ZEROS          TO GFCTGO-COD-SQL-ERRO
               END-IF
               IF  WRK-5000-COD-RETORNO
                                       EQUAL 99
                   MOVE WRK-5000-COD-SQL-ERRO
                                       TO GFCTGO-COD-SQL-ERRO
               END-IF
               MOVE ZEROS              TO GFCTGO-COD-MSG-ERRO
               MOVE 1                  TO GFCTGO-ERRO
               MOVE WRK-5000-DESC-MSG  TO GFCTGO-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       111000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       120000-VERIFICAR-ONLINE         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WRK-5500-AREA-SAIDA.

           MOVE 'GFCT5500'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5500-AREA-SAIDA.

           IF  WRK-5500-COD-RETORNO    NOT EQUAL ZEROS
               IF  WRK-5500-COD-RETORNO
                                       EQUAL 04
                   MOVE 1              TO GFCTGO-ERRO
                   MOVE ZEROS          TO GFCTGO-COD-SQL-ERRO
                   MOVE 0030           TO GFCTGO-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 111000-OBTER-DESC-MENSAGEM
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTGO-DESC-MSG-ERRO
               END-IF
               IF  WRK-5500-COD-RETORNO
                                       EQUAL 99
                   INITIALIZE          GFCT0M-AREA-ERROS
                   MOVE 'GFCT0378'     TO GFCT0M-PROGRAMA
                   MOVE 'DB2'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'PARM_CTRL_GSTAO'
                                       TO GFCT0M-NOME-TAB
                   MOVE 'SELECT'       TO GFCT0M-COMANDO-SQL
                   MOVE WRK-5500-COD-SQL-ERRO
                                       TO GFCT0M-SQLCODE
                                          GFCT0M-SQLCA-AREA
                                          GFCTGO-COD-SQL-ERRO
                   MOVE 9              TO GFCTGO-ERRO
                   MOVE 0012           TO GFCTGO-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   MOVE '0010'         TO GFCT0M-LOCAL
                   MOVE WRK-5500-DESC-MSG
                                       TO GFCTGO-DESC-MSG-ERRO
               END-IF
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       120000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       200000-PROCESSAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           PERFORM 210000-CONSISTIR-DADOS.

           PERFORM 220000-OBTER-DATA-SIST.

           PERFORM 230000-CONSULTAR-GFCTB0A7.

           PERFORM 240000-MOVER-CAMPOS.

      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       210000-CONSISTIR-DADOS          SECTION.
      *----------------------------------------------------------------*

           IF (GFCTGN-TRANSACAO        EQUAL LOW-VALUES OR
               GFCTGN-TRANSACAO        EQUAL SPACES) OR
              (GFCTGN-FUNCAO           EQUAL LOW-VALUES OR
               GFCTGN-FUNCAO           EQUAL SPACES) OR
              (GFCTGN-FUNC-BDSCO       EQUAL LOW-VALUES OR
               GFCTGN-FUNC-BDSCO       EQUAL SPACES) OR
              (GFCTGN-CAGPTO           NOT NUMERIC OR
               GFCTGN-CAGPTO           NOT EQUAL 4 AND 14 AND  9  AND
                                                15 AND 16 AND 17)
               MOVE 0001               TO GFCTGO-ERRO
                                          GFCTGO-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE ZEROS              TO GFCTGO-COD-SQL-ERRO
               PERFORM 111000-OBTER-DESC-MENSAGEM
               MOVE WRK-5000-DESC-MSG  TO GFCTGO-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       210000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       220000-OBTER-DATA-SIST          SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
               SELECT  CSIST_PRINC,
                       DPROCM_ANTER,
                       DPROCM_ATUAL,
                       DPROCM_PROX
               INTO   :GFCTB0A1.CSIST-PRINC,
                      :GFCTB0A1.DPROCM-ANTER,
                      :GFCTB0A1.DPROCM-ATUAL,
                      :GFCTB0A1.DPROCM-PROX
               FROM    DB2PRD.PARM_DATA_PROCM
               WHERE   CSIST_PRINC     = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE 'GFCTB0A1'         TO WRK-NOME-TAB
               PERFORM 221000-TRATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       220000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       221000-TRATAR-ERRO-DB2          SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT0378'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCODE                TO GFCT0M-SQLCODE
                                          GFCTGO-COD-SQL-ERRO.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.
           MOVE 9                      TO GFCTGO-ERRO.

           IF  GFCT0M-COMANDO-SQL      EQUAL 'SELECT'
               MOVE 0012               TO GFCTGO-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
           ELSE
               MOVE 0019               TO GFCTGO-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
           END-IF.

           PERFORM 111000-OBTER-DESC-MENSAGEM.

           MOVE SPACES                 TO GFCTGO-DESC-MSG-ERRO.

           STRING WRK-5000-DESC-MSG ' - '
                  WRK-NOME-TAB
           DELIMITED BY '  '           INTO GFCTGO-DESC-MSG-ERRO.

           PERFORM 300000-FINALIZAR.

      *----------------------------------------------------------------*
       221000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       230000-CONSULTAR-GFCTB0A7       SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGN-CAGPTO          TO CAGPTO-CTA        OF GFCTB0A7.
           MOVE GFCTGN-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB0A7.
           MOVE GFCTGN-DATA-EVENTO     TO DOCOR-EVNTO       OF GFCTB0A7.
           MOVE GFCTGN-HORA            TO HIDTFD-ESTRN-AGPTO
                                                            OF GFCTB0A7.

           EXEC SQL
               SELECT  CAGPTO_CTA,
                       CSERVC_TARIF,
                       DOCOR_EVNTO,
                       HIDTFD_ESTRN_AGPTO,
                       CSIT_SOLTC_ESTRN,
                       CSIT_PROCM_ESTRN,
                       CINDCD_AGPTO_TOT,
                       CDEPDC_SOLTC_ESTRN,
                       CFUNC_SOLTC_ESTRN,
                       HSOLTC_ESTRN,
                       CTPO_OPER_MOTVO,
                       CMOTVO_JUSTF,
                       RJUSTF_SOLTC_ESTRN,
                       CDEPDC_PAREC_ESTRN,
                       CFUNC_PAREC_ESTRN,
                       HPAREC_ESTRN,
                       RJUSTF_PAREC_ESTRN,
                       QESTRN_AGPTO,
                       VESTRN_SOLTD,
                       QCANCT_AGPTO,
                       VCANCT_SOLTD,
                       DEFETV_CREDT_ESTRN
               INTO   :GFCTB0A7.CAGPTO-CTA,
                      :GFCTB0A7.CSERVC-TARIF,
                      :GFCTB0A7.DOCOR-EVNTO,
                      :GFCTB0A7.HIDTFD-ESTRN-AGPTO,
                      :GFCTB0A7.CSIT-SOLTC-ESTRN,
                      :GFCTB0A7.CSIT-PROCM-ESTRN,
                      :GFCTB0A7.CINDCD-AGPTO-TOT,
                      :GFCTB0A7.CDEPDC-SOLTC-ESTRN,
                      :GFCTB0A7.CFUNC-SOLTC-ESTRN,
                      :GFCTB0A7.HSOLTC-ESTRN,
                      :GFCTB0A7.CTPO-OPER-MOTVO,
                      :GFCTB0A7.CMOTVO-JUSTF,
                      :GFCTB0A7.RJUSTF-SOLTC-ESTRN,
                      :GFCTB0A7.CDEPDC-PAREC-ESTRN,
                      :GFCTB0A7.CFUNC-PAREC-ESTRN,
                      :GFCTB0A7.HPAREC-ESTRN,
                      :GFCTB0A7.RJUSTF-PAREC-ESTRN,
                      :GFCTB0A7.QESTRN-AGPTO,
                      :GFCTB0A7.VESTRN-SOLTD,
                      :GFCTB0A7.QCANCT-AGPTO,
                      :GFCTB0A7.VCANCT-SOLTD,
                      :GFCTB0A7.DEFETV-CREDT-ESTRN
               FROM    DB2PRD.PDIDO_ESTRN_GRP
               WHERE   CAGPTO_CTA         = :GFCTB0A7.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0A7.CSERVC-TARIF
                 AND   DOCOR_EVNTO        = :GFCTB0A7.DOCOR-EVNTO
                 AND   HIDTFD_ESTRN_AGPTO = :GFCTB0A7.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PDIDO_ESTRN_GRP'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE 'GFCTB0A7'         TO WRK-NOME-TAB
               PERFORM 221000-TRATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       230000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       240000-MOVER-CAMPOS             SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB0A7 TO GFCTGO-CAGPTO.
           MOVE CSERVC-TARIF OF GFCTB0A7
                                       TO GFCTGO-CSERVC-TARIF.
           MOVE HIDTFD-ESTRN-AGPTO OF GFCTB0A7
                                       TO GFCTGO-HORA.
           MOVE DOCOR-EVNTO OF GFCTB0A7
                                       TO GFCTGO-DATA-EVENTO.
           MOVE CINDCD-AGPTO-TOT OF GFCTB0A7
                                       TO GFCTGO-ESTORNO-TOTAL.
           STRING HIDTFD-ESTRN-AGPTO OF GFCTB0A7(09:02) '.'
                  HIDTFD-ESTRN-AGPTO OF GFCTB0A7(06:02) '.'
                  HIDTFD-ESTRN-AGPTO OF GFCTB0A7(01:04)
           DELIMITED BY SIZE           INTO GFCTGO-DATA-SOLICITACAO.

           MOVE QESTRN-AGPTO OF GFCTB0A7
                                       TO GFCTGO-QESTORNOS.
           MOVE QCANCT-AGPTO OF GFCTB0A7
                                       TO GFCTGO-QCANCELAMENTOS.
           MOVE VESTRN-SOLTD OF GFCTB0A7
                                       TO GFCTGO-VAESTORNAR.
           MOVE VCANCT-SOLTD OF GFCTB0A7
                                       TO GFCTGO-VACANCELAR.
           MOVE RJUSTF-SOLTC-ESTRN-TEXT OF GFCTB0A7
                                       TO GFCTGO-COMENTARIO.
           MOVE RJUSTF-PAREC-ESTRN-TEXT OF GFCTB0A7
                                       TO GFCTGO-JUST.
           MOVE CSIT-PROCM-ESTRN OF GFCTB0A7
                                       TO GFCTGO-SITUACAO.
           MOVE CSIT-SOLTC-ESTRN OF GFCTB0A7
                                       TO GFCTGO-PARECER.

           EVALUATE GFCTGN-CAGPTO
               WHEN 15
                   PERFORM 241000-OBTER-POSTO
               WHEN 14
                   PERFORM 242000-OBTER-AGENCIA
               WHEN 16
                   PERFORM 243000-OBTER-EMPRESA
               WHEN 17
                   PERFORM 244000-OBTER-POSTAL
               WHEN 4
                   PERFORM 245000-OBTER-CLIENTE
               WHEN OTHER
                   PERFORM 246000-OBTER-SEGMENTO
           END-EVALUATE.


           MOVE CTPO-OPER-MOTVO OF GFCTB0A7
                                       TO GFCTGO-COPERACAO.

           PERFORM 247000-OBTER-DESC-OPERACAO.

           MOVE RTPO-OPER-MOTVO OF GFCTB0G0
                                       TO GFCTGO-ROPERACAO.
           MOVE CMOTVO-JUSTF OF GFCTB0A7
                                       TO GFCTGO-CMOTIVO.

           PERFORM 248000-OBTER-DESC-MOTIVO.

           MOVE RMOTVO-JUSTF OF GFCTB089
                                       TO GFCTGO-RMOTIVO.

      *----------------------------------------------------------------*
       240000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       241000-OBTER-POSTO              SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGN-CAGPTO          TO CAGPTO-CTA        OF GFCTB0A8.
           MOVE GFCTGN-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB0A8.
           MOVE GFCTGN-DATA-EVENTO     TO DOCOR-EVNTO       OF GFCTB0A8.
           MOVE GFCTGN-HORA            TO HIDTFD-ESTRN-AGPTO
                                                            OF GFCTB0A8.

           EXEC SQL
               SELECT  CAGPTO_CTA,
                       CSERVC_TARIF,
                       DOCOR_EVNTO,
                       HIDTFD_ESTRN_AGPTO,
                       CEMPR_INC,
                       CDEPDC,
                       CPOSTO_SERVC
               INTO   :GFCTB0A8.CAGPTO-CTA,
                      :GFCTB0A8.CSERVC-TARIF,
                      :GFCTB0A8.DOCOR-EVNTO,
                      :GFCTB0A8.HIDTFD-ESTRN-AGPTO,
                      :GFCTB0A8.CEMPR-INC,
                      :GFCTB0A8.CDEPDC,
                      :GFCTB0A8.CPOSTO-SERVC
               FROM    DB2PRD.PDIDO_ESTRN_PAB
               WHERE   CAGPTO_CTA         = :GFCTB0A8.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0A8.CSERVC-TARIF
                 AND   DOCOR_EVNTO        = :GFCTB0A8.DOCOR-EVNTO
                 AND   HIDTFD_ESTRN_AGPTO = :GFCTB0A8.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PDIDO_ESTRN_PAB'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE 'GFCTB0A8'         TO WRK-NOME-TAB
               PERFORM 221000-TRATAR-ERRO-DB2
           END-IF.

           MOVE CDEPDC OF GFCTB0A8     TO WRK-COD-AGENCIA.

           PERFORM 242100-OBTER-DESC-AGENCIA

           MOVE GFCT1X-DESC-DEPDC(1)   TO GFCTGO-RAGENCIA.

           MOVE CDEPDC OF GFCTB0A8     TO GFCTGO-CAGENCIA.

           MOVE CPOSTO-SERVC OF GFCTB0A8
                                       TO GFCTGO-CPAB.

           IF  CINDCD-AGPTO-TOT OF GFCTB0A7
                                       EQUAL 'S'
               MOVE 'TODOS'            TO GFCTGO-RPAB
           ELSE
               PERFORM 241100-OBTER-DESC-PAB

               MOVE GFCTGC-DESC-POSTO(1)
                                       TO GFCTGO-RPAB
           END-IF.

      *----------------------------------------------------------------*
       241000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       241100-OBTER-DESC-PAB           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGB-ENTRADA.

           MOVE 100                    TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.

           MOVE GFCTGN-TRANSACAO       TO GFCTGB-TRANSACAO.
           MOVE GFCTGN-FUNCAO          TO GFCTGB-FUNCAO.
           MOVE ZEROS                  TO GFCTGB-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTGB-QTDE-TOT-REG.
           MOVE GFCTGN-FUNC-BDSCO      TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTGB-FIM.

           MOVE CDEPDC       OF GFCTB0A8
                                       TO GFCTGB-COD-DEPDC.
           MOVE CPOSTO-SERVC OF GFCTB0A8
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
               MOVE 9                  TO GFCTGO-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG05-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0378'         TO GFCT0M-TRANSACAO

               PERFORM 300000-FINALIZAR
           END-IF

           IF  GFCTGC-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTGO-FIM
               MOVE GFCTGC-ERRO        TO GFCTGO-ERRO
               MOVE GFCTGC-COD-SQL-ERRO
                                       TO GFCTGO-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO
                                       TO GFCTGO-COD-MSG-ERRO

               IF  GFCTGC-ERRO         EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO
                                       TO GFCTGO-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5109-'
                          GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTGO-DESC-MSG-ERRO
               END-IF

               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       241100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       242000-OBTER-AGENCIA            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGN-CAGPTO          TO CAGPTO-CTA        OF GFCTB0A3.
           MOVE GFCTGN-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB0A3.
           MOVE GFCTGN-DATA-EVENTO     TO DOCOR-EVNTO       OF GFCTB0A3.
           MOVE GFCTGN-HORA            TO HIDTFD-ESTRN-AGPTO
                                                            OF GFCTB0A3.

           EXEC SQL
               SELECT  CAGPTO_CTA,
                       CSERVC_TARIF,
                       DOCOR_EVNTO,
                       HIDTFD_ESTRN_AGPTO,
                       CEMPR_INC,
                       CDEPDC
               INTO   :GFCTB0A3.CAGPTO-CTA,
                      :GFCTB0A3.CSERVC-TARIF,
                      :GFCTB0A3.DOCOR-EVNTO,
                      :GFCTB0A3.HIDTFD-ESTRN-AGPTO,
                      :GFCTB0A3.CEMPR-INC,
                      :GFCTB0A3.CDEPDC
               FROM    DB2PRD.PDIDO_ESTRN_AG
               WHERE   CAGPTO_CTA         = :GFCTB0A3.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0A3.CSERVC-TARIF
                 AND   DOCOR_EVNTO        = :GFCTB0A3.DOCOR-EVNTO
                 AND   HIDTFD_ESTRN_AGPTO = :GFCTB0A3.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PDIDO_ESTRN_AG'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE 'GFCTB0A3'         TO WRK-NOME-TAB
               PERFORM 221000-TRATAR-ERRO-DB2
           END-IF.

           MOVE CDEPDC OF GFCTB0A3     TO GFCTGO-CAGENCIA.

           IF  CINDCD-AGPTO-TOT OF GFCTB0A7
                                       EQUAL 'S'
               MOVE 'TODAS'            TO GFCTGO-RAGENCIA
           ELSE
               MOVE CDEPDC OF GFCTB0A3 TO WRK-COD-AGENCIA

               PERFORM 242100-OBTER-DESC-AGENCIA

               MOVE GFCT1X-DESC-DEPDC(1)
                                       TO GFCTGO-RAGENCIA
           END-IF.

      *----------------------------------------------------------------*
       242000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       242100-OBTER-DESC-AGENCIA       SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT1V-ENTRADA.

           MOVE 660                    TO GFCT1V-LL.
           MOVE ZEROS                  TO GFCT1V-ZZ.

           MOVE GFCTGN-TRANSACAO       TO GFCT1V-TRANSACAO.
           MOVE GFCTGN-FUNCAO          TO GFCT1V-FUNCAO.
           MOVE 001                    TO GFCT1V-QTDE-OCOR.
           MOVE ZEROS                  TO GFCT1V-QTDE-TOT-REG.
           MOVE GFCTGN-FUNC-BDSCO      TO GFCT1V-FUNC-BDSCO.
           MOVE 'N'                    TO GFCT1V-FIM.

           MOVE WRK-COD-AGENCIA        TO GFCT1V-COD-DEPDC(1).

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
               MOVE 9                  TO GFCTGO-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG05-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0378'         TO GFCT0M-TRANSACAO

               PERFORM 300000-FINALIZAR
           END-IF

           IF  GFCT1X-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTGO-FIM
               MOVE GFCT1X-ERRO        TO GFCTGO-ERRO
               MOVE GFCT1X-COD-SQL-ERRO
                                       TO GFCTGO-COD-SQL-ERRO
               MOVE GFCT1X-COD-MSG-ERRO
                                       TO GFCTGO-COD-MSG-ERRO

               IF  GFCT1X-ERRO         EQUAL 1
                   MOVE GFCT1X-DESC-MSG-ERRO
                                       TO GFCTGO-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5006-'
                          GFCT1X-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTGO-DESC-MSG-ERRO
               END-IF

               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       242100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       243000-OBTER-EMPRESA            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGN-CAGPTO          TO CAGPTO-CTA        OF GFCTB0A6.
           MOVE GFCTGN-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB0A6.
           MOVE GFCTGN-DATA-EVENTO     TO DOCOR-EVNTO       OF GFCTB0A6.
           MOVE GFCTGN-HORA            TO HIDTFD-ESTRN-AGPTO
                                                            OF GFCTB0A6.

           EXEC SQL
               SELECT  CAGPTO_CTA,
                       CSERVC_TARIF,
                       DOCOR_EVNTO,
                       HIDTFD_ESTRN_AGPTO,
                       CAG_EMPR,
                       CRZ_CTA_EMPR,
                       CNRO_CTA_DEB,
                       CLCTO_CTA_EMPR
               INTO   :GFCTB0A6.CAGPTO-CTA,
                      :GFCTB0A6.CSERVC-TARIF,
                      :GFCTB0A6.DOCOR-EVNTO,
                      :GFCTB0A6.HIDTFD-ESTRN-AGPTO,
                      :GFCTB0A6.CAG-EMPR,
                      :GFCTB0A6.CRZ-CTA-EMPR,
                      :GFCTB0A6.CNRO-CTA-DEB,
                      :GFCTB0A6.CLCTO-CTA-EMPR
               FROM    DB2PRD.PDIDO_ESTRN_EMPR
               WHERE   CAGPTO_CTA         = :GFCTB0A6.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0A6.CSERVC-TARIF
                 AND   DOCOR_EVNTO        = :GFCTB0A6.DOCOR-EVNTO
                 AND   HIDTFD_ESTRN_AGPTO = :GFCTB0A6.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PDIDO_ESTRN_EMPR' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE 'GFCTB0A6'         TO WRK-NOME-TAB
               PERFORM 221000-TRATAR-ERRO-DB2
           END-IF.

           MOVE CAG-EMPR OF GFCTB0A6   TO GFCTGO-CAGENCIA.
           MOVE CNRO-CTA-DEB OF GFCTB0A6
                                       TO GFCTGO-CCONTA.

           PERFORM 243100-OBTER-DESC-EMPRESA.

           MOVE GFCTBS-NOME-CLIENTE(1) TO GFCTGO-RCONTA.

      *----------------------------------------------------------------*
       243000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       243100-OBTER-DESC-EMPRESA       SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTBR-ENTRADA.

           MOVE 100                    TO GFCTBR-LL.
           MOVE ZEROS                  TO GFCTBR-ZZ.

           MOVE GFCTGN-TRANSACAO       TO GFCTBR-TRANSACAO.
           MOVE GFCTGN-FUNCAO          TO GFCTBR-FUNCAO.
           MOVE ZEROS                  TO GFCTBR-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTBR-QTDE-TOT-REG.
           MOVE GFCTGN-FUNC-BDSCO      TO GFCTBR-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTBR-FIM.

           MOVE CAG-EMPR OF GFCTB0A6   TO GFCTBR-COD-DEPDC.
           MOVE CNRO-CTA-DEB OF GFCTB0A6
                                       TO GFCTBR-CCTA-CLI.

           MOVE SPACES                 TO GFCTBS-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCTBS-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5060'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTBR-ENTRADA
                                             GFCTBS-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTGO-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG05-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0378'         TO GFCT0M-TRANSACAO

               PERFORM 300000-FINALIZAR
           END-IF

           IF  GFCTBS-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTGO-FIM
               MOVE GFCTBS-ERRO        TO GFCTGO-ERRO
               MOVE GFCTBS-COD-SQL-ERRO
                                       TO GFCTGO-COD-SQL-ERRO
               MOVE GFCTBS-COD-MSG-ERRO
                                       TO GFCTGO-COD-MSG-ERRO

               IF  GFCTBS-ERRO         EQUAL 1
                   MOVE GFCTBS-DESC-MSG-ERRO
                                       TO GFCTGO-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5060-'
                          GFCTBS-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTGO-DESC-MSG-ERRO
               END-IF

               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       243100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       244000-OBTER-POSTAL             SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGN-CAGPTO          TO CAGPTO-CTA        OF GFCTB0A9.
           MOVE GFCTGN-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB0A9.
           MOVE GFCTGN-DATA-EVENTO     TO DOCOR-EVNTO       OF GFCTB0A9.
           MOVE GFCTGN-HORA            TO HIDTFD-ESTRN-AGPTO
                                                            OF GFCTB0A9.

           EXEC SQL
               SELECT  CAGPTO_CTA,
                       CSERVC_TARIF,
                       DOCOR_EVNTO,
                       HIDTFD_ESTRN_AGPTO,
                       CEMPR_INC,
                       CDEPDC,
                       CPOSTO_SERVC
               INTO   :GFCTB0A9.CAGPTO-CTA,
                      :GFCTB0A9.CSERVC-TARIF,
                      :GFCTB0A9.DOCOR-EVNTO,
                      :GFCTB0A9.HIDTFD-ESTRN-AGPTO,
                      :GFCTB0A9.CEMPR-INC,
                      :GFCTB0A9.CDEPDC,
                      :GFCTB0A9.CPOSTO-SERVC
               FROM    DB2PRD.PDIDO_ESTRN_PSTAL
               WHERE   CAGPTO_CTA         = :GFCTB0A9.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0A9.CSERVC-TARIF
                 AND   DOCOR_EVNTO        = :GFCTB0A9.DOCOR-EVNTO
                 AND   HIDTFD_ESTRN_AGPTO = :GFCTB0A9.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PDIDO_ESTRN_PSTAL'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE 'GFCTB0A9'         TO WRK-NOME-TAB
               PERFORM 221000-TRATAR-ERRO-DB2
           END-IF.

           MOVE CDEPDC OF GFCTB0A9     TO WRK-COD-AGENCIA.

           PERFORM 242100-OBTER-DESC-AGENCIA

           MOVE GFCT1X-DESC-DEPDC(1)   TO GFCTGO-RAGENCIA.

           MOVE CDEPDC OF GFCTB0A9     TO GFCTGO-CAGENCIA.

           MOVE CPOSTO-SERVC OF GFCTB0A9
                                       TO GFCTGO-CPAB.

           IF  CINDCD-AGPTO-TOT OF GFCTB0A7
                                       EQUAL 'S'
               MOVE 'TODOS'            TO GFCTGO-RPAB
           ELSE
               PERFORM 244100-OBTER-DESC-POSTAL

               MOVE GFCTGC-DESC-POSTO(1)
                                       TO GFCTGO-RPAB
           END-IF.

      *----------------------------------------------------------------*
       244000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       244100-OBTER-DESC-POSTAL        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGB-ENTRADA.

           MOVE 100                    TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.

           MOVE GFCTGN-TRANSACAO       TO GFCTGB-TRANSACAO.
           MOVE GFCTGN-FUNCAO          TO GFCTGB-FUNCAO.
           MOVE ZEROS                  TO GFCTGB-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTGB-QTDE-TOT-REG.
           MOVE GFCTGN-FUNC-BDSCO      TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTGB-FIM.

           MOVE CDEPDC       OF GFCTB0A9
                                       TO GFCTGB-COD-DEPDC.
           MOVE CPOSTO-SERVC OF GFCTB0A9
                                       TO GFCTGB-COD-POSTO.

           MOVE SPACES                 TO GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCTGC-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5090'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTGB-ENTRADA
                                             GFCTGC-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTGO-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG05-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0378'         TO GFCT0M-TRANSACAO

               PERFORM 300000-FINALIZAR
           END-IF

           IF  GFCTGC-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTGO-FIM
               MOVE GFCTGC-ERRO        TO GFCTGO-ERRO
               MOVE GFCTGC-COD-SQL-ERRO
                                       TO GFCTGO-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO
                                       TO GFCTGO-COD-MSG-ERRO

               IF  GFCTGC-ERRO         EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO
                                       TO GFCTGO-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5090-'
                          GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTGO-DESC-MSG-ERRO
               END-IF

               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       244100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       245000-OBTER-CLIENTE            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGN-CAGPTO          TO CAGPTO-CTA        OF GFCTB0A4.
           MOVE GFCTGN-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB0A4.
           MOVE GFCTGN-DATA-EVENTO     TO DOCOR-EVNTO       OF GFCTB0A4.
           MOVE GFCTGN-HORA            TO HIDTFD-ESTRN-AGPTO
                                                            OF GFCTB0A4.

           EXEC SQL
               SELECT  CAGPTO_CTA,
                       CSERVC_TARIF,
                       DOCOR_EVNTO,
                       HIDTFD_ESTRN_AGPTO,
ST2506                 CCGC_CPF,
ST2506                 CFLIAL_CGC,
ST2506                 CCTRL_CPF_CGC
ST2506                 CCGC_CPF_ST,
ST2506                 CFLIAL_CGC_ST,
ST2506                 CCTRL_CPF_CGC_ST
               INTO   :GFCTB0A4.CAGPTO-CTA,
                      :GFCTB0A4.CSERVC-TARIF,
                      :GFCTB0A4.DOCOR-EVNTO,
                      :GFCTB0A4.HIDTFD-ESTRN-AGPTO,
ST2506                :GFCTB0A4.CCGC-CPF,
ST2506                :GFCTB0A4.CFLIAL-CGC,
ST2506                :GFCTB0A4.CCTRL-CPF-CGC
ST2506                :GFCTB0A4.CCGC-CPF-ST,
ST2506                :GFCTB0A4.CFLIAL-CGC-ST,
ST2506                :GFCTB0A4.CCTRL-CPF-CGC-ST
               FROM    DB2PRD.PDIDO_ESTRN_CLI
               WHERE   CAGPTO_CTA         = :GFCTB0A4.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0A4.CSERVC-TARIF
                 AND   DOCOR_EVNTO        = :GFCTB0A4.DOCOR-EVNTO
                 AND   HIDTFD_ESTRN_AGPTO = :GFCTB0A4.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PDIDO_ESTRN_CLI'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0080'             TO GFCT0M-LOCAL
               MOVE 'GFCTB0A4'         TO WRK-NOME-TAB
               PERFORM 221000-TRATAR-ERRO-DB2
           END-IF.

ST2506*    MOVE CCGC-CPF OF GFCTB0A4   TO GFCTGO-CGC-CPF.
ST2506*    MOVE CFLIAL-CGC OF GFCTB0A4 TO GFCTGO-FILIAL.
ST2506*    MOVE CCTRL-CPF-CGC OF GFCTB0A4
ST2506*                                TO GFCTGO-CONTROLE.
ST2506     MOVE CCGC-CPF-ST            OF GFCTB0A4   
ST2506                                 TO GFCTGO-CGC-CPF.
ST2506     MOVE CFLIAL-CGC-ST          OF GFCTB0A4 
ST2506                                 TO GFCTGO-FILIAL.
ST2506     MOVE CCTRL-CPF-CGC-ST       OF GFCTB0A4
ST2506                                 TO GFCTGO-CONTROLE.

           PERFORM 245100-OBTER-DESC-CLIENTE.

           MOVE GFCTIV-NOME-CLIENTE    TO GFCTGO-RCONTA.

      *----------------------------------------------------------------*
       245000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       245100-OBTER-DESC-CLIENTE       SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTIU-ENTRADA.

           INITIALIZE                  GFCTIU-ENTRADA.

           MOVE 100                    TO GFCTIU-LL.
           MOVE ZEROS                  TO GFCTIU-ZZ.

           MOVE GFCTGN-TRANSACAO       TO GFCTIU-TRANSACAO.
           MOVE GFCTGN-FUNCAO          TO GFCTIU-FUNCAO.
           MOVE GFCTGN-FUNC-BDSCO      TO GFCTIU-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTIU-FIM.
ST2506*    MOVE CCGC-CPF OF GFCTB0A4   TO GFCTIU-NRO-CPF-CNPJ.
ST2506*    MOVE CFLIAL-CGC OF GFCTB0A4 TO GFCTIU-NRO-FILIAL.
ST2506*    MOVE CCTRL-CPF-CGC OF GFCTB0A4
ST2506*                                TO GFCTIU-CTRL-CPF-CNPJ.
ST2506     MOVE CCGC-CPF-ST            OF GFCTB0A4   
ST2506                                 TO GFCTIU-COD-CPF-CNPJ.
ST2506     MOVE CFLIAL-CGC-ST          OF GFCTB0A4 
ST2506                                 TO GFCTIU-COD-FILIAL.
ST2506     MOVE CCTRL-CPF-CGC-ST       OF GFCTB0A4
ST2506                                 TO GFCTIU-CTRL-CPF-CNPJ.

           MOVE SPACES                 TO GFCTIV-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCTIV-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5110'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTIU-ENTRADA
                                             GFCTIV-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTGO-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG05-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0378'         TO GFCT0M-TRANSACAO

               PERFORM 300000-FINALIZAR
           END-IF

           IF  GFCTIV-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTGO-FIM
               MOVE GFCTIV-ERRO        TO GFCTGO-ERRO
               MOVE GFCTIV-COD-SQL-ERRO
                                       TO GFCTGO-COD-SQL-ERRO
               MOVE GFCTIV-COD-MSG-ERRO
                                       TO GFCTGO-COD-MSG-ERRO

               IF  GFCTIV-ERRO         EQUAL 1
                   MOVE GFCTIV-DESC-MSG-ERRO
                                       TO GFCTGO-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5110-'
                          GFCTIV-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTGO-DESC-MSG-ERRO
               END-IF

               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       245100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       246000-OBTER-SEGMENTO           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGN-CAGPTO          TO CAGPTO-CTA        OF GFCTB0B0.
           MOVE GFCTGN-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB0B0.
           MOVE GFCTGN-DATA-EVENTO     TO DOCOR-EVNTO       OF GFCTB0B0.
           MOVE GFCTGN-HORA            TO HIDTFD-ESTRN-AGPTO
                                                            OF GFCTB0B0.

           EXEC SQL
               SELECT  CAGPTO_CTA,
                       CSERVC_TARIF,
                       DOCOR_EVNTO,
                       HIDTFD_ESTRN_AGPTO,
                       CSGMTO_GSTAO_TARIF
               INTO   :GFCTB0B0.CAGPTO-CTA,
                      :GFCTB0B0.CSERVC-TARIF,
                      :GFCTB0B0.DOCOR-EVNTO,
                      :GFCTB0B0.HIDTFD-ESTRN-AGPTO,
                      :GFCTB0B0.CSGMTO-GSTAO-TARIF
                          :WRK-CSGMTO-GSTAO-TARIF-NULL
               FROM    DB2PRD.PDIDO_ESTRN_SGMTO
               WHERE   CAGPTO_CTA         = :GFCTB0B0.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0B0.CSERVC-TARIF
                 AND   DOCOR_EVNTO        = :GFCTB0B0.DOCOR-EVNTO
                 AND   HIDTFD_ESTRN_AGPTO = :GFCTB0B0.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PDIDO_ESTRN_SGMTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0120'             TO GFCT0M-LOCAL
               MOVE 'GFCTB0B0'         TO WRK-NOME-TAB
               PERFORM 221000-TRATAR-ERRO-DB2
           END-IF.

           IF  WRK-CSGMTO-GSTAO-TARIF-NULL
                                       LESS ZEROS
               GO                      TO 246000-99-FIM
           END-IF.

           MOVE CSGMTO-GSTAO-TARIF OF GFCTB0B0
                                       TO GFCTGO-CSEGMENTO.

           PERFORM 246100-OBTER-DESC-SEGMENTO.

           MOVE ISGMTO-GSTAO-TARIF OF GFCTB0H5
                                       TO GFCTGO-RSEGMENTO.

      *----------------------------------------------------------------*
       246000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       246100-OBTER-DESC-SEGMENTO      SECTION.
      *----------------------------------------------------------------*

           MOVE CSGMTO-GSTAO-TARIF OF GFCTB0B0
                                       TO CSGMTO-GSTAO-TARIF
                                                            OF GFCTB0H5.

           EXEC SQL
               SELECT  ISGMTO_GSTAO_TARIF
               INTO   :GFCTB0H5.ISGMTO-GSTAO-TARIF
               FROM    DB2PRD.TTPO_SGMTO_GSTAO
               WHERE   CSGMTO_GSTAO_TARIF
                                       = :GFCTB0H5.CSGMTO-GSTAO-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -811) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'TTPO_SGMTO_GSTAO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0130'             TO GFCT0M-LOCAL
               MOVE 'GFCTB0H5'         TO WRK-NOME-TAB
               PERFORM 221000-TRATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       246100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       247000-OBTER-DESC-OPERACAO      SECTION.
      *----------------------------------------------------------------*

           MOVE CTPO-OPER-MOTVO OF GFCTB0A7
                                       TO CTPO-OPER-MOTVO OF GFCTB0G0.

           EXEC SQL
               SELECT  RTPO_OPER_MOTVO
               INTO   :GFCTB0G0.RTPO-OPER-MOTVO
               FROM    DB2PRD.TPO_OPER_MOTVO
               WHERE   CTPO_OPER_MOTVO = :GFCTB0G0.CTPO-OPER-MOTVO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'TPO_OPER_MOTVO'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0140'             TO GFCT0M-LOCAL
               MOVE 'GFCTB0G0'         TO WRK-NOME-TAB
               PERFORM 221000-TRATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       247000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       248000-OBTER-DESC-MOTIVO        SECTION.
      *----------------------------------------------------------------*

           MOVE CTPO-OPER-MOTVO OF GFCTB0A7
                                       TO CTPO-OPER-MOTVO OF GFCTB089.
           MOVE CMOTVO-JUSTF OF GFCTB0A7
                                       TO CMOTVO-JUSTF OF GFCTB089.

           EXEC SQL
               SELECT  RMOTVO_JUSTF
               INTO   :GFCTB089.RMOTVO-JUSTF
               FROM    DB2PRD.MOTVO_JUSTF_OPER
               WHERE   CTPO_OPER_MOTVO = :GFCTB089.CTPO-OPER-MOTVO
                 AND   CMOTVO_JUSTF    = :GFCTB089.CMOTVO-JUSTF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'MOTVO_JUSTF_OPER' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0150'             TO GFCT0M-LOCAL
               MOVE 'GFCTB089'         TO WRK-NOME-TAB
               PERFORM 221000-TRATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       248000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       300000-FINALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
