       ID  DIVISION.

       PROGRAM-ID.  BVVE6400.

      *REMARKS.
      *****************************************************************
      * PROGRAMADOR  : SIDNEY                       - CPM-BRAXIS      *
      * SUPERVISOR   : BENE                         - CPM-BRAXIS      *
      * ANALISTA     : SIDNEY                       - CPM-BRAXIS      *
      * DATA         : 23.07.2008                                     *
      *                                                               *
      * OBJETIVO : ATUALIZA DADOS DO RETORNO DA BASE DE TRANSFERENCIA *
      *            DOMICILIO                                          *
      *            RATUTRAD - INPUT  - I#BVVEG6 - 149 - FB            *
      *                                                               *
      * TABELA       : BVVEB025  ->  DB2PRD.TSOLTC_TRAVA_DOMCL        *
      *================================================================*
062010*                        A L T E R A C A O                       *
      *----------------------------------------------------------------*
      *    ANALISTA: PROCWORK                                          *
      *    DATA....: 06/2010                                           *
      *----------------------------------------------------------------*
      *    OBJETIVO: INCLUSAO DO CAMPO TIPO DE DOMICILIO (BANDEIRA) NA *
      *              TABELA BVVEB025.                                  *
      *              - AJUSTE DO TAMANHO DO ARQUIVO                    *
      *              - INCLUSAO DO CAMPO NA CHAVE DE ATUALIZ.DE SOLIC. *
      *================================================================*
      *
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT RATUTRAD       ASSIGN TO UT-S-RATUTRAD
                  FILE STATUS    IS WRK-FS-RATUTRAD.
           EJECT
       DATA DIVISION.

       FILE SECTION.

       FD  RATUTRAD
           BLOCK 0
           RECORDING F
           LABEL RECORD STANDARD.

       01  REG-RATUTRAD.
           03  FILLER           PIC X(151).

           EJECT

       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*
       77  FILLER                      PIC X(32)        VALUE
           '* INICIO DA WORKING BVVE6400 *'.

       77  WRK-FS-RATUTRAD             PIC  X(02)        VALUE SPACES.
       77  WRK-FS-SAIDA                PIC  X(02)        VALUE SPACES.

       77  WRK-ABERTURA                PIC X(13) VALUE  'NA ABERTURA'.
       77  WRK-LEITURA                 PIC X(13) VALUE  'NA LEITURA'.
       77  WRK-GRAVACAO                PIC X(13) VALUE  'NA GRAVACAO'.
       77  WRK-FECHAMENTO              PIC X(13) VALUE  'NO FECHAMENTO'.
       77  WRK-MENSAGEM                PIC X(50) VALUE SPACES.

       77  WRK-BATCH                   PIC  X(08) VALUE 'BATCH'.
       77  WRK-ABEND                   PIC S9(04) VALUE +1111 COMP.

       77  ACU-LIDOS-RATUTRAD          PIC  9(11)        VALUE ZEROS.
       77  ACU-LIDOS-BVVEB025          PIC  9(11)        VALUE ZEROS.

       COPY 'I#BVVEG6'.

       COPY 'I#BRAD7C'.

       77  FILLER                      PIC  X(32)          VALUE
           '*   MENSAGEM ERRO FILE-STATUS  *'.
      *----------------------------------------------------------------*
       01  WRK-MSG-FS-ERRO.
           03  FILLER                  PIC  X(07)          VALUE SPACES.
           03  FILLER                  PIC  X(02)          VALUE '* '.
           03  FILLER                  PIC  X(05)          VALUE 'ERRO'.
           03  WRK-OPERACAO            PIC  X(13)          VALUE SPACES.
           03  FILLER                  PIC  X(12)          VALUE
               ' DO ARQUIVO '.
           03  WRK-NOME-ARQ            PIC  X(08)          VALUE SPACES.
           03  FILLER                  PIC  X(17)          VALUE
               ' - FILE-STATUS = '.
           03  WRK-FILE-STATUS         PIC  X(02)          VALUE SPACES.
           03  FILLER                  PIC  X(02)          VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)        VALUE
           '* AREA DE CHECKPOINT/RESTART *'.
      *----------------------------------------------------------------*

       01  WRK-CKRS0100                PIC X(008)     VALUE 'CKRS0100'.

       01  WRK-VARIAVEIS-AUXILIARES.
7C2511     05 WRK-TEMP-CNPJ-X          PIC X(09).
7C2511     05 WRK-TEMP-CNPJ-9 REDEFINES WRK-TEMP-CNPJ-X
7C2511                                  PIC 9(09).
7C2511     05 WRK-TEMP-FILIAL-X        PIC X(04).
7C2511     05 WRK-TEMP-FILIAL-9 REDEFINES WRK-TEMP-FILIAL-X
7C2511                                  PIC 9(04).

       01  WRK-AREA-RESTART-BVVE6400.
7C2511    05 WRK-CCNPJ-CPF-ESTBL       PIC X(09) VALUE SPACES.
7C2511*    05 WRK-CCNPJ-CPF-ESTBL-N     PIC S9(09) COMP-3 VALUE ZEROS.
7C2511    05 WRK-CFLIAL-CNPJ-ESTBL     PIC X(04) VALUE SPACES.
7C2511*    05 WRK-CFLIAL-CNPJ-ESTBL-N   PIC S9(05) COMP-3 VALUE ZEROS.
7C2511    05 FILLER                    PIC X(01) VALUE SPACES.
          05 WRK-CTPO-VDA-ESTBL        PIC S9(9) COMP-3 VALUE ZEROS.
          05 WRK-CTRAVA-DOMCL-COML     PIC X(01) VALUE SPACES.
          05 WRK-CTPO-OPER-FINCR       PIC X(01) VALUE SPACES.
062010    05 WRK-CTPO-BANDEIRA         PIC X(02) VALUE SPACES.
          05 WRK-DINIC-TRAVA-DOMCL     PIC X(10) VALUE SPACES.
          05 WRK-RESTO                 PIC X(162) VALUE SPACES.
          05 WRK-LID-RATUTRAD          PIC 9(11) COMP-3 VALUE ZEROS.
          05 WRK-INC-BVVEB025          PIC 9(11) COMP-3 VALUE ZEROS.

       COPY 'I#CKRS01'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(22)        VALUE
           '* ACUMULADORES*'.
      *----------------------------------------------------------------*

       01  ACU-LID-BVVEB025           PIC 9(11) COMP-3 VALUE ZEROS.
       01  ACU-INC-BVVEB025           PIC 9(11) COMP-3 VALUE ZEROS.

       01  WRK-EDIT1                  PIC ZZ.ZZZ.ZZZ.ZZ9 VALUE ZEROS.
       01  WRK-EDIT2                  PIC ZZ.ZZZ.ZZZ.ZZ9 VALUE ZEROS.

       01  BVVEB025-NULL.
           05  DFNAL-TRAVA-PCERO-NULL PIC S9(04) COMP VALUE ZEROS.

      *---------------------------------------------------------------*
      *    WORK-AREA PARA BRAD7600                                    *
      *---------------------------------------------------------------*

       01  DATA-HORA.
           03 DT-JULIANA          PIC 9(05) COMP-3.
           03 DT-AAMMDD           PIC 9(07) COMP-3.
           03 DT-AAAAMMDD         PIC 9(09) COMP-3.
           03 TI-HHMMSS           PIC 9(07) COMP-3.
           03 TI-HHMMSSMMMMMM     PIC 9(13) COMP-3.
           03 TIMESTAMP           PIC X(20).

      *---------------------------------------------------------------*
      *    FIM DA WORK-AREA DO BRAD7600                               *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    AREA-AUXILIAR                                              *
      *---------------------------------------------------------------*

       01  WRK-DATA-AUX                PIC X(10).
       01  WRK-DATA REDEFINES WRK-DATA-AUX.
           03  WRK-DIA                 PIC 9(02).
           03  WRK-PONTO1              PIC X(01).
           03  WRK-MES                 PIC 9(02).
           03  WRK-PONTO2              PIC X(01).
           03  WRK-ANO                 PIC 9(04).

       01  WRK-DATA-AUZ                PIC 9(08).
       01  WRK-DTA  REDEFINES WRK-DATA-AUZ.
           03  ANO-AUZ                 PIC 9(04).
           03  MES-AUZ                 PIC 9(02).
           03  DIA-AUZ                 PIC 9(02).


      *----------------------------------------------------------------*
      *      A R E A   D E   I N C L U D E   D A S   T A B E L A S     *
      *----------------------------------------------------------------*

      *--------------------- AREA DE RETORNO SQL ----------------------*

           EXEC SQL
                INCLUDE  SQLCA
           END-EXEC.

      *--------------- TABELA DB2PRD.TSOLTC_TRAVA_DOMCL ---------------*

           EXEC SQL
                INCLUDE  BVVEB025
           END-EXEC.

       77  FILLER                      PIC X(32)        VALUE
           '*  FIM DA WORKING BVVE6400 *'.
      *----------------------------------------------------------------*

           EJECT

       PROCEDURE DIVISION.

           CALL 'CKRS1000'.

       0000-INICIO                 SECTION.
      *---------------------------------------------------------------*

           PERFORM 0100-INICIAR

           PERFORM 0200-PROCESSAR  UNTIL
                           WRK-FS-RATUTRAD EQUAL '10'.

           PERFORM 0300-FINALIZAR.

       0000-99-FIM.  EXIT.
      *---------------------------------------------------------------*
       0100-INICIAR                SECTION.
      *---------------------------------------------------------------*

           PERFORM 0110-INICIO-CHECK.

           OPEN    INPUT   RATUTRAD

           MOVE    WRK-ABERTURA        TO   WRK-OPERACAO.
           IF  WRK-FS-RATUTRAD  NOT EQUAL  '00'
               MOVE   'APL'                TO  ERR-TIPO-ACESSO
               MOVE   'RATUTRAD '          TO  WRK-NOME-ARQ
               MOVE    WRK-FS-RATUTRAD     TO  WRK-FILE-STATUS
               MOVE    WRK-MSG-FS-ERRO     TO  ERR-TEXTO
               PERFORM 99999-ROTINA-ERRO
           END-IF.

           PERFORM 0150-LER-RATUTRAD

           MOVE    WRK-LEITURA         TO   WRK-OPERACAO.

           IF  WRK-FS-RATUTRAD    EQUAL  '10'  AND
               ACU-LIDOS-RATUTRAD EQUAL  ZEROS
               DISPLAY '*********** BVVE6400 ************'
               DISPLAY '*                               *'
               DISPLAY '*  ARQUIVO RATUTRAD ESTA VAZIO  *'
               DISPLAY '*                               *'
               DISPLAY '*********** BVVE6400 ************'
           END-IF.

       0100-99-FIM.  EXIT.

           EJECT
      *----------------------------------------------------------------*
      *                   INICIO CHECKPOINT/RESTART                   *
      *---------------------------------------------------------------*
       0110-INICIO-CHECK            SECTION.
      *---------------------------------------------------------------*


           INITIALIZE  CKRS01-INTERFACE.

      *****************************************************************
      ** FAZEMOS A CHAMADA CKRS0100, MODO INICIALIZACAO               *
      *****************************************************************

           MOVE  ' '                   TO CK01-PLAN.
           MOVE  'DB2 '                TO CK01-ID-DB2.
           MOVE  'I'                   TO CK01-FUNCAO.
           MOVE  WRK-AREA-RESTART-BVVE6400
                                       TO CK01-AREA-RESTART.
           MOVE  LENGTH OF WRK-AREA-RESTART-BVVE6400
                                       TO CK01-TAM-AREA-RESTART.

           PERFORM 0120-CHAMADA-CKRS0100

           IF  CK01-STATUS EQUAL       TO 'REST'
                MOVE CK01-AREA-RESTART(1:CK01-TAM-AREA-RESTART)
                                       TO WRK-AREA-RESTART-BVVE6400
                MOVE WRK-LID-RATUTRAD  TO ACU-LIDOS-BVVEB025
                MOVE WRK-INC-BVVEB025  TO ACU-INC-BVVEB025
           END-IF.

       0110-99-FIM. EXIT.
      *---------------------------------------------------------------*
       0120-CHAMADA-CKRS0100        SECTION.
      *---------------------------------------------------------------*

           CALL WRK-CKRS0100           USING CKRS01-INTERFACE

           IF  CK01-CODIGO-RETORNO     NOT EQUAL ZEROS
               DISPLAY
               '***************** BVVE6400 **********************'
               DISPLAY
               '*                                               *'
               DISPLAY
               '* ERRO NO MODULO CKRS0100 - FUNCAO = ' CK01-FUNCAO
                                                     '          *'
               DISPLAY
               '*                                               *'
               DISPLAY
               '*************************************************'
               PERFORM 99999-ROTINA-ERRO
           END-IF.

       0120-99-FIM. EXIT.
      *---------------------------------------------------------------*
       0130-ROTINA-CHECK               SECTION.
      *---------------------------------------------------------------*

           MOVE  'P'                   TO CK01-FUNCAO
           MOVE  'DB2 '                TO CK01-ID-DB2.
           PERFORM 0120-CHAMADA-CKRS0100.

      *---------------------------------------------------------------*
       0130-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
       0140-FINALIZA-CHECK          SECTION.
      *---------------------------------------------------------------*

      ****************************************************************
      * FAZEMOS CHAMADA CKRS0100, OPCAO F (FINALIZA), PARA FINALIZAR *
      * A CONEXAO COM O MODULO DE CHECKPOINT/RESTART                 *
      ****************************************************************

           MOVE  'F'                   TO CK01-FUNCAO.
           MOVE  'DB2 '                TO CK01-ID-DB2.

           PERFORM 0120-CHAMADA-CKRS0100.

       0140-99-FIM. EXIT.
      *---------------------------------------------------------------*
       0150-LER-RATUTRAD            SECTION.
      *---------------------------------------------------------------*

           READ    RATUTRAD INTO BVVEG6-REG.

           IF  WRK-FS-RATUTRAD EQUAL  '10'
               GO 0150-99-FIM
           END-IF.

           IF  WRK-FS-RATUTRAD  NOT EQUAL  '00'
               MOVE   WRK-LEITURA          TO   WRK-OPERACAO
               MOVE   'APL'                TO  ERR-TIPO-ACESSO
               MOVE   'RATUTRAD '          TO  WRK-NOME-ARQ
               MOVE    WRK-FS-RATUTRAD     TO  WRK-FILE-STATUS
               MOVE    WRK-MSG-FS-ERRO     TO  ERR-TEXTO
               PERFORM 99999-ROTINA-ERRO
           END-IF.

           ADD     1               TO  ACU-LIDOS-RATUTRAD.

       0150-99-FIM. EXIT.
      *---------------------------------------------------------------*
       0200-PROCESSAR               SECTION.
      *---------------------------------------------------------------*

           PERFORM 0210-MONTA-CHV-BVVEB025.

           PERFORM 0230-ATUALIZA-TAB-BVVEB025.

           PERFORM 0150-LER-RATUTRAD.

       0200-99-FIM. EXIT.
      *---------------------------------------------------------------*
       0210-MONTA-CHV-BVVEB025     SECTION.
      *---------------------------------------------------------------*

7C2511*    CONVERTE CNPJ ALFANUMERICO PARA NUMERICO
7C2511     MOVE BVVEG6-CNPJCPF-NRO      TO WRK-CCNPJ-CPF-ESTBL.
7C2511     MOVE WRK-CCNPJ-CPF-ESTBL   TO CCNPJ-CPF-ESTBL-ST OF BVVEB025.
7C2511*    CONVERTE FILIAL ALFANUMERICA PARA NUMERICA
7C2511     MOVE BVVEG6-CNPJ-FILIAL        TO WRK-CFLIAL-CNPJ-ESTBL.
7C2511     MOVE WRK-CFLIAL-CNPJ-ESTBL TO CFLIAL-ESTBL-ST OF BVVEB025.

           MOVE    BVVEG6-PONTO-VENDA   TO        CPTO-VDA-ESTBL
                                        OF        BVVEB025.

           MOVE    BVVEG6-IND-TRAVA     TO        CTRAVA-DOMCL-COML
                                        OF        BVVEB025.

           MOVE    BVVEG6-TIPO-OPER     TO        CTPO-OPER-FINCR
                                        OF        BVVEB025.

062010     MOVE    BVVEG6-TPO-BANDEIRA  TO        CIDTFD-BANDE-ESTBL
062010                                  OF        BVVEB025.

           MOVE    BVVEG6-DTINIC-TRAVA  TO        WRK-DATA-AUZ.
           MOVE    DIA-AUZ              TO        WRK-DIA.
           MOVE    MES-AUZ              TO        WRK-MES.
           MOVE    ANO-AUZ              TO        WRK-ANO.
           MOVE    '.'                  TO        WRK-PONTO1
                                                  WRK-PONTO2.
           MOVE    WRK-DATA-AUX         TO        DINIC-TRAVA-DOMCL
                                        OF        BVVEB025.

       0210-99-FIM. EXIT.
      *---------------------------------------------------------------*
       0230-ATUALIZA-TAB-BVVEB025   SECTION.
      *---------------------------------------------------------------*

           IF  BVVEG6-COD-RETORNO EQUAL ZEROS
               MOVE 'A'                      TO CSIT-SOLTC-PCERO
                                             OF BVVEB025
               MOVE BVVEG6-COD-RETORNO       TO CAVISO-RETOR-TRAVA
                                             OF BVVEB025
               MOVE BVVEG6-MSG-ERRO          TO RMSGEM-RETOR-TRAVA
                                             OF BVVEB025
               MOVE BVVEG6-DTTRAVA-REDECARD  TO WRK-DATA-AUZ
               MOVE DIA-AUZ                  TO WRK-DIA
               MOVE MES-AUZ                  TO WRK-MES
               MOVE ANO-AUZ                  TO WRK-ANO
               MOVE    '.'                   TO WRK-PONTO1
                                                WRK-PONTO2
               MOVE WRK-DATA-AUX             TO DFNAL-TRAVA-PCERO
                                             OF BVVEB025
               MOVE ZEROS                    TO DFNAL-TRAVA-PCERO-NULL
           ELSE
               MOVE 'I'                      TO CSIT-SOLTC-PCERO
                                             OF BVVEB025
               MOVE BVVEG6-COD-RETORNO       TO CAVISO-RETOR-TRAVA
                                             OF BVVEB025
               MOVE BVVEG6-MSG-ERRO          TO RMSGEM-RETOR-TRAVA
                                             OF BVVEB025
               MOVE -1                       TO DFNAL-TRAVA-PCERO-NULL.

           EXEC   SQL
            UPDATE DB2PRD.TSOLTC_TRAVA_DOMCL
             SET
               CSIT_SOLTC_PCERO     = :BVVEB025.CSIT-SOLTC-PCERO,
               CAVISO_RETOR_TRAVA   = :BVVEB025.CAVISO-RETOR-TRAVA,
               RMSGEM_RETOR_TRAVA   = :BVVEB025.RMSGEM-RETOR-TRAVA,
               DFNAL_TRAVA_PCERO    = :BVVEB025.DFNAL-TRAVA-PCERO
                                      :DFNAL-TRAVA-PCERO-NULL
             WHERE
                 CCNPJ_CPF_ESTBL     = :BVVEB025.CCNPJ-CPF-ESTBL   AND
                 CFLIAL_CNPJ_ESTBL   = :BVVEB025.CFLIAL-CNPJ-ESTBL AND
                 CPTO_VDA_ESTBL      = :BVVEB025.CPTO-VDA-ESTBL    AND
                 CTRAVA_DOMCL_COML   = :BVVEB025.CTRAVA-DOMCL-COML AND
                 CTPO_OPER_FINCR     = :BVVEB025.CTPO-OPER-FINCR   AND
062010           CIDTFD_BANDE_ESTBL  = :BVVEB025.CIDTFD-BANDE-ESTBL AND
                 DINIC_TRAVA_DOMCL   = :BVVEB025.DINIC-TRAVA-DOMCL
           END-EXEC.

           IF  ( SQLCODE  NOT =  +100 AND ZEROS )
           OR  ( SQLWARN0     =  'W' )
                   MOVE 'DB2'                 TO ERR-TIPO-ACESSO
                   MOVE 'TSOLTC_TRAVA_DOMCL ' TO ERR-DBD-TAB
                   MOVE 'SELECT'              TO ERR-FUN-COMANDO
                   MOVE SQLCODE               TO ERR-SQL-CODE
                   MOVE 0001                  TO ERR-LOCAL
                   MOVE SPACES                TO ERR-SEGM
                   PERFORM 99999-ROTINA-ERRO.

           IF  ( SQLCODE   =  +100 )
                   MOVE 'DB2'                 TO ERR-TIPO-ACESSO
                   MOVE 'TSOLTC_TRAVA_DOMCL ' TO ERR-DBD-TAB
                   MOVE 'SELECT'              TO ERR-FUN-COMANDO
                   MOVE SQLCODE               TO ERR-SQL-CODE
                   MOVE 0002                  TO ERR-LOCAL
                   MOVE SPACES                TO ERR-SEGM
                   PERFORM 99999-ROTINA-ERRO.

           IF    SQLCODE  EQUAL ZEROS
                 ADD 1                        TO ACU-INC-BVVEB025
           END-IF.

           MOVE CCNPJ-CPF-ESTBL   OF BVVEB025
                                       TO WRK-CCNPJ-CPF-ESTBL.
           MOVE CFLIAL-CNPJ-ESTBL OF BVVEB025
                                       TO WRK-CFLIAL-CNPJ-ESTBL.
           MOVE CPTO-VDA-ESTBL    OF BVVEB025
                                       TO WRK-CTPO-VDA-ESTBL.
           MOVE CTRAVA-DOMCL-COML OF BVVEB025
                                       TO WRK-CTRAVA-DOMCL-COML.
           MOVE CTPO-OPER-FINCR   OF BVVEB025
                                       TO WRK-CTPO-OPER-FINCR.
062010     MOVE CIDTFD-BANDE-ESTBL OF BVVEB025
062010                                 TO WRK-CTPO-BANDEIRA.

           MOVE DINIC-TRAVA-DOMCL OF BVVEB025
                                       TO WRK-DINIC-TRAVA-DOMCL.
           MOVE SPACES                 TO WRK-RESTO
           MOVE ACU-LIDOS-RATUTRAD     TO WRK-LID-RATUTRAD
           MOVE ACU-INC-BVVEB025       TO WRK-INC-BVVEB025
           MOVE WRK-AREA-RESTART-BVVE6400
                                       TO CK01-AREA-RESTART.
           MOVE  LENGTH OF WRK-AREA-RESTART-BVVE6400
                                       TO CK01-TAM-AREA-RESTART.
           PERFORM 0130-ROTINA-CHECK.

       0230-99-FIM. EXIT.
      *----------------------------------------------------------------*
      *     EXIBIR TOTAIS, FECHAR ARQUIVOS E ECNERRAR PROGRAMA         *
      *----------------------------------------------------------------*
       0300-FINALIZAR                SECTION.
      *---------------------------------------------------------------*

           MOVE ACU-LIDOS-RATUTRAD     TO WRK-EDIT1.
           MOVE ACU-INC-BVVEB025       TO WRK-EDIT2.

           DISPLAY '****************** BVVE6400 *********************'.
           DISPLAY '** RATUTRAD - REGS LIDOS.......: 'WRK-EDIT1   '**'.
           DISPLAY '** BVVEB025 - REGS ATUALIZADOS.: 'WRK-EDIT2   '**'.
           DISPLAY '****************** BVVE6400 *********************'.

           CLOSE  RATUTRAD.

           MOVE    WRK-FECHAMENTO      TO   WRK-OPERACAO.

           IF  WRK-FS-RATUTRAD     NOT EQUAL  '00'
               MOVE   'APL'                TO  ERR-TIPO-ACESSO
               MOVE   'RATUTRAD '          TO  WRK-NOME-ARQ
               MOVE    WRK-FS-RATUTRAD     TO  WRK-FILE-STATUS
               MOVE    WRK-MSG-FS-ERRO     TO  ERR-TEXTO
               PERFORM 99999-ROTINA-ERRO
           END-IF.

           PERFORM 0140-FINALIZA-CHECK.

           STOP    RUN.

       0300-99-FIM. EXIT.
      *---------------------------------------------------------------*
       99999-ROTINA-ERRO           SECTION.
      *-----------------------------------------------------------

           MOVE   'BVVE6400'           TO  ERR-PGM.

           CALL   'BRAD7100'   USING   WRK-BATCH
                                       ERRO-AREA.
           GOBACK.

       99999-99-FIM.                   EXIT.
