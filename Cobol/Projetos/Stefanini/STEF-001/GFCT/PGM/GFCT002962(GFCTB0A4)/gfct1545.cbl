      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1545.
       AUTHOR.     FLAVIO AUGUSTO MARIA.
      *================================================================*
      *                    C P M - S I S T E M A S                     *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1545                                    *
      *    PROGRAMADOR.:   FLAVIO AUGUSTO MARIA    - CPM PATO BRANCO   *
      *    ANALISTA CPM:   ADRIANO SANTANA         - CPM PATO BRANCO   *
      *    ANALISTA....:   MARCELO CREMM           - PWI / GP. 50      *
      *    DATA........:   21/12/2005                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   TRATAR CALCULO DE AGRUPAMENTO DE CLIENTE DO *
      *        DIA. CALCULA A SOMATORIA DE CANCELADOS E SOMATORIA DE   *
      *        ESTORNOS A SER EFETUADO POR CGC-CPF.                    *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                       INCLUDE/BOOK      *
      *                    DB2PRD.PARM_DATA_PROCM      GFCTB0A1        *
      *                    DB2PRD.PDIDO_ESTRN_CLI      GFCTB0A4        *
      *                    DB2PRD.PDIDO_ESTRN_GRP      GFCTB0A7        *
      *                    DB2PRD.MOVTO_EVNTO_CRRTT    GFCTB092        *
      *                    DB2PRD.V01CLIENTE_AGENCIA   CLIEV004        *
      *                    DB2PRD.V01CTA_POUPANCA      CLIEV007        *
      *                    DB2PRD.V01CTA_CORRENTE      CLIEV008        *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERRO PELA POOL7100.      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL7100 - MODULO PARA CANCELAMENTO DO PROGRAMA.            *
      *================================================================*
      *   ULTIMA ALTERACAO:                                            *
      *   =================                                            *
      *                                                                *
      *   MARCELO CREMM - PROCWORK - SETEMBRO-2006                     *
      *                                                                *
      *   ===> PASSAR A TRATAR TAMBEM CSIT-EVNTO-RECBD = 15, 16 E 17.  *
      *   ===> TRATAR TAMBEM OS RELATORIOS.                            *
      *                                                                *
      *================================================================*
080208*----------------------------------------------------------------*
080208*              SONDA PROCWORK - CONSULTORIA - ALTERACAO          *
080208*----------------------------------------------------------------*
080208*    ANALISTA....:   VINICIUS                         / GRUPO 50 *
080208*    DATA........:   08/02/2008                                  *
080208*    OBJETIVO....:   INCLUSAO DE TRATAMENTO PARA SITUACOES:      *
080208*                    18, 19 E NN.                                *
080208*----------------------------------------------------------------*

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
           '*** GFCT1545 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACUMULADORES ***'.
      *----------------------------------------------------------------*

       77  ACU-OCORRENCIAS             PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-QTD-CANCELA             PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-QTD-ESTORNO             PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-VLR-CANCELA             PIC  9(010)V9(2) COMP-3
                                                           VALUE ZEROS.
       77  ACU-VLR-ESTORNO             PIC  9(010)V9(2) COMP-3
                                                           VALUE ZEROS.
       77  ACU-QTD-CANCELA-TOT         PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-QTD-ESTORNO-TOT         PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-VLR-CANCELA-TOT         PIC  9(010)V9(2) COMP-3
                                                           VALUE ZEROS.
       77  ACU-VLR-ESTORNO-TOT         PIC  9(010)V9(2) COMP-3
                                                           VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-FIM-C1                  PIC  X(001)         VALUE SPACES.
       77  WRK-FIM-C2                  PIC  X(001)         VALUE SPACES.
       77  WRK-FIM-C3                  PIC  X(001)         VALUE SPACES.
       77  WRK-FIM-C4                  PIC  X(001)         VALUE SPACES.
       77  WRK-FIM-C5                  PIC  X(001)         VALUE SPACES.
       77  WRK-MASCARA-VL              PIC  Z.ZZZ.ZZZ.ZZ9,99.
       77  WRK-MASCARA                 PIC  ZZZZZZZZ9.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB044
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A4
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A7
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB092
           END-EXEC.

           EXEC SQL
               INCLUDE CLIEV004
           END-EXEC.

           EXEC SQL
               INCLUDE CLIEV007
           END-EXEC.

           EXEC SQL
               INCLUDE CLIEV008
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA DECLARACAO DE CURSORES ***'.
      *----------------------------------------------------------------*

           EXEC SQL
            DECLARE CSR01-GFCTB0A7 CURSOR WITH HOLD FOR
             SELECT A.CAGPTO_CTA,
                    A.CSERVC_TARIF,
                    A.DOCOR_EVNTO,
                    A.HIDTFD_ESTRN_AGPTO,
                    A.CSIT_SOLTC_ESTRN,
                    A.CSIT_PROCM_ESTRN,
                    A.CINDCD_AGPTO_TOT,
                    A.CDEPDC_SOLTC_ESTRN,
                    A.CFUNC_SOLTC_ESTRN,
                    A.HSOLTC_ESTRN,
                    A.CTPO_OPER_MOTVO,
                    A.CMOTVO_JUSTF,
                    A.RJUSTF_SOLTC_ESTRN,
                    A.CDEPDC_PAREC_ESTRN,
                    A.CFUNC_PAREC_ESTRN,
                    A.HPAREC_ESTRN,
                    A.RJUSTF_PAREC_ESTRN,
                    A.QESTRN_AGPTO,
                    A.VESTRN_SOLTD,
                    A.QCANCT_AGPTO,
                    A.VCANCT_SOLTD,
                    A.DEFETV_CREDT_ESTRN,
                    B.CAGPTO_CTA,
                    B.CSERVC_TARIF,
                    B.DOCOR_EVNTO,
                    B.HIDTFD_ESTRN_AGPTO,
                    B.CCGC_CPF,
                    B.CFLIAL_CGC,
                    B.CCTRL_CPF_CGC
               FROM DB2PRD.PDIDO_ESTRN_GRP A,
                    DB2PRD.PDIDO_ESTRN_CLI B
              WHERE A.CAGPTO_CTA         = B.CAGPTO_CTA
                AND A.CSERVC_TARIF       = B.CSERVC_TARIF
                AND A.DOCOR_EVNTO        = B.DOCOR_EVNTO
                AND A.HIDTFD_ESTRN_AGPTO = B.HIDTFD_ESTRN_AGPTO
                AND A.CAGPTO_CTA         IN (4)
                AND A.CSIT_SOLTC_ESTRN   IN (1)
                AND A.CSIT_PROCM_ESTRN   IN (2)
              ORDER BY B.CAGPTO_CTA,
                       B.CSERVC_TARIF,
                       B.DOCOR_EVNTO,
      *ST2506 - ALTERACAO CNPJ ALPHA NUMERICO
                       B.CCGC_CPF,
                       B.CFLIAL_CGC,
                       B.CCTRL_CPF_CGC,
ST25X6                 B.CCGC_CPF_ST,
ST25X6                 B.CFLIAL_CGC_ST,
ST25X6                 B.CCTRL_CPF_CGC_ST
      *ST2506 - FIM
           END-EXEC.

           EXEC SQL
            DECLARE CSR02-CLIEV004 CURSOR WITH HOLD FOR
             SELECT CID_CLI
               FROM DB2PRD.V01CLIENTE_AGENCIA
              WHERE CCGC_CPF           = :CLIEV004.CCGC-CPF   AND
                    CFLIAL_CGC         = :CLIEV004.CFLIAL-CGC AND
                    CCTRL_CPF_CGC      = :CLIEV004.CCTRL-CPF-CGC
           END-EXEC.

           EXEC SQL
            DECLARE CSR03-CLIEV007 CURSOR WITH HOLD FOR
             SELECT CJUNC_DEPDC,
                    CCTA_CLI
               FROM DB2PRD.V01CTA_POUPANCA
              WHERE CID_CLI            = :CLIEV007.CID-CLI
           END-EXEC.

           EXEC SQL
            DECLARE CSR04-CLIEV008 CURSOR WITH HOLD FOR
             SELECT CJUNC_DEPDC,
                    CCTA_CLI
               FROM DB2PRD.V01CTA_CORRENTE
              WHERE CID_CLI            = :CLIEV008.CID-CLI
           END-EXEC.

           EXEC SQL
            DECLARE CSR05-GFCTB092 CURSOR WITH HOLD FOR
             SELECT DENVIO_MOVTO_TARIF,
                    CROTNA_ORIGE_MOVTO,
                    CNRO_ARQ_MOVTO,
                    CSEQ_MOVTO,
                    CSERVC_TARIF,
                    CMIDIA_SERVC_MOVTO,
                    CPERIF_SERVC_MOVTO,
                    QEVNTO_AGRUP,
                    HPREST_SERVC_MOVTO,
                    CTPO_CTA_MOVTO,
                    CBCO_DSTNO_MOVTO,
                    CAG_DSTNO_MOVTO,
                    CCTA_DSTNO_MOVTO,
                    CPAB_DSTNO_MOVTO,
                    CAG_CTLZA_DEB,
                    CCTA_CTLZA_DEB,
                    VTARIF_BRUTO_MOVTO,
                    VTARIF_LIQ_MOVTO,
                    VTARIF_DEB_MOVTO,
                    DAGNDA_DEB_MOVTO,
                    DEFETV_DEB_MOVTO,
                    QMAX_TENTV_DEB,
                    QTENTV_DEB_MOVTO,
                    CSIT_EVNTO_RECBD,
                    CIDTFD_PGMC_COBR,
                    DOCOR_EVNTO,
                    VTARIF_CADTR_SIST
               FROM DB2PRD.MOVTO_EVNTO_CRRTT
              WHERE CSERVC_TARIF       = :GFCTB092.CSERVC-TARIF     AND
                    DOCOR_EVNTO        = :GFCTB092.DOCOR-EVNTO      AND
                    CAG_DSTNO_MOVTO    = :GFCTB092.CAG-DSTNO-MOVTO  AND
                    CCTA_DSTNO_MOVTO   = :GFCTB092.CCTA-DSTNO-MOVTO
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA POOL7100 ***'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** GFCT1545 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       000000-INICIAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 100000-INICIALIZAR.

           PERFORM 200000-VERIFICAR-VAZIO.

           PERFORM 300000-PROCESSAR    UNTIL
                   WRK-FIM-C1          EQUAL 'S'.

           PERFORM 400000-FECHAR-CURSOR1.

           PERFORM 500000-FINALIZAR.

      *----------------------------------------------------------------*
       000000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           PERFORM 110000-ACESSAR-GFCTB0A1.

      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       110000-ACESSAR-GFCTB0A1         SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
             SELECT CSIST_PRINC,
                    DPROCM_ANTER,
                    DPROCM_ATUAL,
                    DPROCM_PROX
               INTO :GFCTB0A1.CSIST-PRINC,
                    :GFCTB0A1.DPROCM-ANTER,
                    :GFCTB0A1.DPROCM-ATUAL,
                    :GFCTB0A1.DPROCM-PROX
               FROM DB2PRD.PARM_DATA_PROCM
              WHERE CSIST_PRINC        = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0010'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       200000-VERIFICAR-VAZIO          SECTION.
      *----------------------------------------------------------------*

           PERFORM 210000-ABRIR-CURSOR1.

           PERFORM 220000-LER-CURSOR1.

           IF  WRK-FIM-C1              EQUAL 'S'
               DISPLAY '************** GFCT1545 **************'
               DISPLAY '*                                    *'
               DISPLAY '*        NAO HA CALCULOS POR         *'
               DISPLAY '*      AGRUPAMENTO CLIENTE PARA      *'
               DISPLAY '*       MOVIMENTO: '
                           DPROCM-ATUAL OF GFCTB0A1 '        *'
               DISPLAY '*                                    *'
               DISPLAY '************** GFCT1545 **************'
           END-IF.

      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       210000-ABRIR-CURSOR1            SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                       TO WRK-FIM-C1.

           EXEC SQL
               OPEN CSR01-GFCTB0A7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_ESTRN_GRP ' TO ERR-DBD-TAB
               MOVE 'OPEN    '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0020'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       210000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       220000-LER-CURSOR1              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              FETCH CSR01-GFCTB0A7
               INTO
                   :GFCTB0A7.CAGPTO-CTA,
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
                   :GFCTB0A7.DEFETV-CREDT-ESTRN,
                   :GFCTB0A4.CAGPTO-CTA,
                   :GFCTB0A4.CSERVC-TARIF,
                   :GFCTB0A4.DOCOR-EVNTO,
                   :GFCTB0A4.HIDTFD-ESTRN-AGPTO,
                   :GFCTB0A4.CCGC-CPF,
                   :GFCTB0A4.CFLIAL-CGC,
                   :GFCTB0A4.CCTRL-CPF-CGC,
ST25X6             :GFCTB0A4.CCGC-CPF-ST,
ST25X6             :GFCTB0A4.CFLIAL-CGC-ST,
ST25X6             :GFCTB0A4.CCTRL-CPF-CGC-ST
      *ST2506 - FIM
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_ESTRN_GRP ' TO ERR-DBD-TAB
               MOVE 'FETCH   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0030'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-C1
           ELSE
               ADD 1                   TO ACU-OCORRENCIAS
           END-IF.

      *----------------------------------------------------------------*
       220000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       300000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 310000-ABRIR-CURSOR2.

           PERFORM 320000-LER-CURSOR2.

           PERFORM 330000-PROCESSAR-CURSOR2 UNTIL
               WRK-FIM-C2                   EQUAL 'S'.

           PERFORM 340000-FECHAR-CURSOR2.

           PERFORM 350000-EFETUAR-UPDATE.

           PERFORM 220000-LER-CURSOR1.

      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       310000-ABRIR-CURSOR2            SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                       TO WRK-FIM-C2.
ST25X6*    MOVE CCGC-CPF      OF GFCTB0A4 TO CCGC-CPF      OF CLIEV004.
ST25X6*    MOVE CFLIAL-CGC    OF GFCTB0A4 TO CFLIAL-CGC    OF CLIEV004.
ST25X6*    MOVE CCTRL-CPF-CGC OF GFCTB0A4 TO CCTRL-CPF-CGC OF CLIEV004.
ST25X6     MOVE CCGC-CPF-ST   OF GFCTB0A4 TO CCGC-CPF      OF CLIEV004.
ST25X6     MOVE CFLIAL-CGC-ST OF GFCTB0A4 TO CFLIAL-CGC    OF CLIEV004.
ST25X6     MOVE CCTRL-CPF-CGC-ST OF GFCTB0A4 
                                          TO CCTRL-CPF-CGC OF CLIEV004.

           EXEC SQL
               OPEN CSR02-CLIEV004
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'V01CLIENTE_AGENCIA'  TO ERR-DBD-TAB
               MOVE 'OPEN    '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0040'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       310000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       320000-LER-CURSOR2              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              FETCH CSR02-CLIEV004
               INTO :CLIEV004.CID-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'V01CLIENTE_AGENCIA'  TO ERR-DBD-TAB
               MOVE 'FETCH   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0050'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-C2
           END-IF.

      *----------------------------------------------------------------*
       320000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       330000-PROCESSAR-CURSOR2        SECTION.
      *----------------------------------------------------------------*

           PERFORM 331000-PROCESSAR-CLIEV007.
           PERFORM 332000-PROCESSAR-CLIEV008.

           PERFORM 320000-LER-CURSOR2.

      *----------------------------------------------------------------*
       330000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       331000-PROCESSAR-CLIEV007       SECTION.
      *----------------------------------------------------------------*

           PERFORM 331100-ABRIR-CURSOR3.

           PERFORM 331200-LER-CURSOR3.

           PERFORM 331300-PROCESSAR-CURSOR3 UNTIL
               WRK-FIM-C3                   EQUAL 'S'.

           PERFORM 331400-FECHAR-CURSOR3.

      *----------------------------------------------------------------*
       331000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       331100-ABRIR-CURSOR3            SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM-C3.
           MOVE CID-CLI OF CLIEV004    TO CID-CLI OF CLIEV007.

           EXEC SQL
               OPEN CSR03-CLIEV007
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'V01CTA_POUPANCA ' TO ERR-DBD-TAB
               MOVE 'OPEN    '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0060'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       331100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       331200-LER-CURSOR3              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              FETCH CSR03-CLIEV007
               INTO :CLIEV007.CJUNC-DEPDC,
                    :CLIEV007.CCTA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'V01CTA_POUPANCA ' TO ERR-DBD-TAB
               MOVE 'FETCH   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0070'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-C3
           END-IF.

      *----------------------------------------------------------------*
       331200-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       331300-PROCESSAR-CURSOR3        SECTION.
      *----------------------------------------------------------------*

           MOVE CJUNC-DEPDC OF CLIEV007 TO CAG-DSTNO-MOVTO  OF GFCTB092.
           MOVE CCTA-CLI    OF CLIEV007 TO CCTA-DSTNO-MOVTO OF GFCTB092.

           PERFORM 331310-ABRIR-CURSOR5.

           PERFORM 331320-LER-CURSOR5.

           PERFORM 331330-PROCESSAR-CURSOR5  UNTIL
               WRK-FIM-C5                    EQUAL 'S'.

           PERFORM 331340-FECHAR-CURSOR5.

           PERFORM 331200-LER-CURSOR3.

      *----------------------------------------------------------------*
       331300-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       331310-ABRIR-CURSOR5            SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM-C5.
           MOVE CSERVC-TARIF OF GFCTB0A7 TO CSERVC-TARIF OF GFCTB092.
           MOVE DOCOR-EVNTO  OF GFCTB0A7 TO DOCOR-EVNTO  OF GFCTB092.

           EXEC SQL
               OPEN CSR05-GFCTB092
           END-EXEC.

           IF (SQLCODE                   NOT EQUAL ZEROS) OR
              (SQLWARN0                  EQUAL 'W'      )
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'MOVTO_EVNTO_CRRTT'  TO ERR-DBD-TAB
               MOVE 'OPEN    '           TO ERR-FUN-COMANDO
               MOVE  SQLCODE             TO ERR-SQL-CODE
               MOVE '0080'               TO ERR-LOCAL
               MOVE  SPACES              TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       331310-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       331320-LER-CURSOR5              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              FETCH CSR05-GFCTB092
               INTO :GFCTB092.DENVIO-MOVTO-TARIF,
                    :GFCTB092.CROTNA-ORIGE-MOVTO,
                    :GFCTB092.CNRO-ARQ-MOVTO,
                    :GFCTB092.CSEQ-MOVTO,
                    :GFCTB092.CSERVC-TARIF,
                    :GFCTB092.CMIDIA-SERVC-MOVTO,
                    :GFCTB092.CPERIF-SERVC-MOVTO,
                    :GFCTB092.QEVNTO-AGRUP,
                    :GFCTB092.HPREST-SERVC-MOVTO,
                    :GFCTB092.CTPO-CTA-MOVTO,
                    :GFCTB092.CBCO-DSTNO-MOVTO,
                    :GFCTB092.CAG-DSTNO-MOVTO,
                    :GFCTB092.CCTA-DSTNO-MOVTO,
                    :GFCTB092.CPAB-DSTNO-MOVTO,
                    :GFCTB092.CAG-CTLZA-DEB,
                    :GFCTB092.CCTA-CTLZA-DEB,
                    :GFCTB092.VTARIF-BRUTO-MOVTO,
                    :GFCTB092.VTARIF-LIQ-MOVTO,
                    :GFCTB092.VTARIF-DEB-MOVTO,
                    :GFCTB092.DAGNDA-DEB-MOVTO,
                    :GFCTB092.DEFETV-DEB-MOVTO,
                    :GFCTB092.QMAX-TENTV-DEB,
                    :GFCTB092.QTENTV-DEB-MOVTO,
                    :GFCTB092.CSIT-EVNTO-RECBD,
                    :GFCTB092.CIDTFD-PGMC-COBR,
                    :GFCTB092.DOCOR-EVNTO,
                    :GFCTB092.VTARIF-CADTR-SIST
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'MOVTO_EVNTO_CRRTT' TO ERR-DBD-TAB
               MOVE 'FETCH   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0090'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-C5
           END-IF.

      *----------------------------------------------------------------*
       331320-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       331330-PROCESSAR-CURSOR5        SECTION.
      *----------------------------------------------------------------*

080208     IF          CSIT-EVNTO-RECBD    OF GFCTB092
080208                         EQUAL 4 OR 5 OR 6 OR 10
080208         IF          CSIT-EVNTO-RECBD OF GFCTB092
080208                         EQUAL 6
080208             PERFORM 331331-OBTER-VLR-ESTORNADO
080208             COMPUTE VTARIF-DEB-MOVTO        OF GFCTB092
080208                         = (VTARIF-DEB-MOVTO OF GFCTB092
080208                         -  VRTEIO-VLR-SOLTD OF GFCTB044)
080208         END-IF
080208         ADD 1                       TO ACU-QTD-ESTORNO
080208         ADD VTARIF-DEB-MOVTO OF GFCTB092
080208                                     TO ACU-VLR-ESTORNO
080208     ELSE
080208         IF CSIT-EVNTO-RECBD     OF GFCTB092
080208                                 EQUAL 7 OR 8 OR 13
080208             NEXT SENTENCE
080208         ELSE
080208             ADD 1               TO ACU-QTD-CANCELA
080208             ADD VTARIF-BRUTO-MOVTO  OF GFCTB092
080208                                 TO ACU-VLR-CANCELA
080208     END-IF.

           PERFORM 331320-LER-CURSOR5.

      *----------------------------------------------------------------*
       331330-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       331331-OBTER-VLR-ESTORNADO      SECTION.
      *----------------------------------------------------------------*

           MOVE DENVIO-MOVTO-TARIF     OF GFCTB092
                                       TO DENVIO-MOVTO-TARIF
                                       OF GFCTB044.
           MOVE CROTNA-ORIGE-MOVTO     OF GFCTB092
                                       TO CROTNA-ORIGE-MOVTO
                                       OF GFCTB044.
           MOVE CNRO-ARQ-MOVTO         OF GFCTB092
                                       TO CNRO-ARQ-MOVTO OF GFCTB044.
           MOVE CSEQ-MOVTO             OF GFCTB092
                                       TO CSEQ-MOVTO     OF GFCTB044.

           EXEC SQL
             SELECT  SUM(VRTEIO_VLR_SOLTD)
               INTO  :GFCTB044.VRTEIO-VLR-SOLTD
               FROM  DB2PRD.ESTRN_COM_REG
              WHERE  DENVIO_MOVTO_TARIF = :GFCTB044.DENVIO-MOVTO-TARIF
                AND  CROTNA_ORIGE_MOVTO = :GFCTB044.CROTNA-ORIGE-MOVTO
                AND  CNRO_ARQ_MOVTO     = :GFCTB044.CNRO-ARQ-MOVTO
                AND  CSEQ_MOVTO         = :GFCTB044.CSEQ-MOVTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'ESTRN_COM_REG'    TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0101'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       331331-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       331340-FECHAR-CURSOR5           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR05-GFCTB092
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'MOVTO_EVNTO_CRRTT' TO ERR-DBD-TAB
               MOVE 'CLOSE   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0100'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       331340-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       331400-FECHAR-CURSOR3           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              CLOSE CSR03-CLIEV007
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'V01CTA_POUPANCA ' TO ERR-DBD-TAB
               MOVE 'CLOSE   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0110'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       331400-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       332000-PROCESSAR-CLIEV008       SECTION.
      *----------------------------------------------------------------*

           PERFORM 332100-ABRIR-CURSOR4.

           PERFORM 332200-LER-CURSOR4.

           PERFORM 332300-PROCESSAR-CURSOR4  UNTIL
               WRK-FIM-C4                    EQUAL 'S'.

           PERFORM 332400-FECHAR-CURSOR4.

      *----------------------------------------------------------------*
       332000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       332100-ABRIR-CURSOR4            SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM-C4.
           MOVE CID-CLI  OF CLIEV004   TO CID-CLI OF CLIEV008.

           EXEC SQL
               OPEN CSR04-CLIEV008
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'V01CTA_CORRENTE ' TO ERR-DBD-TAB
               MOVE 'OPEN    '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0120'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       332100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       332200-LER-CURSOR4              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              FETCH CSR04-CLIEV008
               INTO :CLIEV008.CJUNC-DEPDC,
                    :CLIEV008.CCTA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'V01CTA_CORRENTE ' TO ERR-DBD-TAB
               MOVE 'FETCH   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0130'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-C4
           END-IF.

      *----------------------------------------------------------------*
       332200-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       332300-PROCESSAR-CURSOR4        SECTION.
      *----------------------------------------------------------------*

           MOVE CJUNC-DEPDC OF CLIEV008 TO CAG-DSTNO-MOVTO  OF GFCTB092.
           MOVE CCTA-CLI    OF CLIEV008 TO CCTA-DSTNO-MOVTO OF GFCTB092.

           PERFORM 331310-ABRIR-CURSOR5.

           PERFORM 331320-LER-CURSOR5.

           PERFORM 331330-PROCESSAR-CURSOR5  UNTIL
               WRK-FIM-C5                    EQUAL 'S'.

           PERFORM 331340-FECHAR-CURSOR5.

           PERFORM 332200-LER-CURSOR4.

      *----------------------------------------------------------------*
       332300-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       332400-FECHAR-CURSOR4           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              CLOSE CSR04-CLIEV008
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'V01CTA_CORRENTE ' TO ERR-DBD-TAB
               MOVE 'CLOSE   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0140'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       332400-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       340000-FECHAR-CURSOR2           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              CLOSE CSR02-CLIEV004
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'V01CLIENTE_AGENCIA' TO ERR-DBD-TAB
               MOVE 'CLOSE   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0150'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       340000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       350000-EFETUAR-UPDATE           SECTION.
      *----------------------------------------------------------------*

           MOVE 3                      TO CSIT-PROCM-ESTRN OF GFCTB0A7.
           MOVE DPROCM-ATUAL           OF GFCTB0A1 TO DEFETV-CREDT-ESTRN
                                                           OF GFCTB0A7.
           MOVE ACU-QTD-ESTORNO        TO QESTRN-AGPTO     OF GFCTB0A7.
           MOVE ACU-VLR-ESTORNO        TO VESTRN-SOLTD     OF GFCTB0A7.
           MOVE ACU-QTD-CANCELA        TO QCANCT-AGPTO     OF GFCTB0A7.
           MOVE ACU-VLR-CANCELA        TO VCANCT-SOLTD     OF GFCTB0A7.

           EXEC SQL
             UPDATE  DB2PRD.PDIDO_ESTRN_GRP
                SET  QESTRN_AGPTO       = :GFCTB0A7.QESTRN-AGPTO,
                     DEFETV_CREDT_ESTRN = :GFCTB0A7.DEFETV-CREDT-ESTRN,
                     VESTRN_SOLTD       = :GFCTB0A7.VESTRN-SOLTD,
                     QCANCT_AGPTO       = :GFCTB0A7.QCANCT-AGPTO,
                     VCANCT_SOLTD       = :GFCTB0A7.VCANCT-SOLTD,
                     CSIT_PROCM_ESTRN   = :GFCTB0A7.CSIT-PROCM-ESTRN
              WHERE  CAGPTO_CTA         = :GFCTB0A7.CAGPTO-CTA     AND
                     CSERVC_TARIF       = :GFCTB0A7.CSERVC-TARIF   AND
                     DOCOR_EVNTO        = :GFCTB0A7.DOCOR-EVNTO    AND
                     HIDTFD_ESTRN_AGPTO = :GFCTB0A7.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_ESTRN_GRP ' TO ERR-DBD-TAB
               MOVE 'UPDATE  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0160'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           ADD ACU-QTD-ESTORNO         TO ACU-QTD-ESTORNO-TOT.
           ADD ACU-QTD-CANCELA         TO ACU-QTD-CANCELA-TOT.
           ADD ACU-VLR-CANCELA         TO ACU-VLR-CANCELA-TOT.
           ADD ACU-VLR-ESTORNO         TO ACU-VLR-ESTORNO-TOT.

           MOVE ZEROS                  TO ACU-QTD-ESTORNO
                                          ACU-VLR-ESTORNO
                                          ACU-QTD-CANCELA
                                          ACU-VLR-CANCELA.

      *----------------------------------------------------------------*
       350000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       400000-FECHAR-CURSOR1           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-GFCTB0A7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_ESTRN_GRP ' TO ERR-DBD-TAB
               MOVE 'CLOSE   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0170'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       400000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       500000-FINALIZAR                SECTION.
      *----------------------------------------------------------------*

           IF  ACU-OCORRENCIAS         GREATER ZEROS
               PERFORM 510000-EXECUTAR-COMMIT
           END-IF.

           PERFORM 520000-EMITIR-DISPLAY.

           GOBACK.

      *----------------------------------------------------------------*
       500000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       510000-EXECUTAR-COMMIT          SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               COMMIT
           END-EXEC.

           IF  SQLCODE                 NOT EQUAL ZEROS
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'COMMIT'           TO ERR-DBD-TAB
                                          ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0180'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       510000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       520000-EMITIR-DISPLAY           SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-OCORRENCIAS        TO WRK-MASCARA.

           DISPLAY '******************** GFCT1545 ********************'.
           DISPLAY '*                                                *'.
           DISPLAY '* QUANTIDADE DE CGC-CPF SOLICITADOS:   '
                                                       WRK-MASCARA ' *'.
           MOVE ACU-QTD-ESTORNO-TOT    TO WRK-MASCARA.

           DISPLAY '* QUANTIDADE DE ESTORNOS     :         '
                                                       WRK-MASCARA ' *'.
           MOVE ACU-VLR-ESTORNO-TOT    TO WRK-MASCARA-VL.

           DISPLAY '* VALOR DE ESTORNOS          :  '
                                                    WRK-MASCARA-VL ' *'.
           MOVE ACU-QTD-CANCELA-TOT    TO WRK-MASCARA.

           DISPLAY '* QUANTIDADE DE CANCELAMENTOS:         '
                                                       WRK-MASCARA ' *'.
           MOVE ACU-VLR-CANCELA-TOT    TO WRK-MASCARA-VL.

           DISPLAY '* VALOR DE CANCELAMENTOS     :  '
                                                    WRK-MASCARA-VL ' *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT1545 ********************'.

      *----------------------------------------------------------------*
       520000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       999999-PROCESSAR-ROTINA-ERRO    SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1545'             TO ERR-PGM.

           IF  ERR-TIPO-ACESSO         EQUAL 'APL'
               CALL 'POOL7100'         USING WRK-BATCH
                                             ERRO-AREA
           ELSE
               CALL 'POOL7100'         USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       999999-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
