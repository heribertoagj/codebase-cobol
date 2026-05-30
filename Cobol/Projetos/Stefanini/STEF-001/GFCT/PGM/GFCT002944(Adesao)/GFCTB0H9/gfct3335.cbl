      *===============================================================*
       IDENTIFICATION DIVISION.
      *===============================================================*

       PROGRAM-ID. GFCT3335.

      *================================================================*
      *                  SONDA PROCWORK INFORMATICA                    *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA.........: GFCT3335                                 *
      *    PROGRAMADOR......: FABIO VISCONDE          - SONDA PROCWORK *
      *    ANALISTA.........: FABIO VISCONDE                           *
      *    DATA.............: 22/06/2010                               *
      *                                                                *
      *----------------------------------------------------------------*
      *                                                                *
      *    OBJETIVO.........: COMPLEMENTACAO DO ARQUIVO DE INTERFACE   *
      *                       PARA O SIGB, COM OS DADOS DE CPF-CNPJ    *
      *                       OBTIDOS DO CLIENTE.                      *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                            INCLUDE/BOOK            *
      *      CONTASEL                            I#SIGB05              *
      *      CLIEADES                            I#GFCTRU              *
      *      EXCESIGB                            I#SIGB05              *
      *      ATLZSIGB                            I#SIGB05              *
      *                                                                *
      *    BANCO DE DADOS...:                                          *
      *      DB2                                                       *
      *      TABLE                             INCLUDE/BOOK            *
      *      V01CLIENTE_AGENCIA                CLIEV004                *
      *      V01CTA_POUPANCA                   CLIEV007                *
      *      V01CTA_CORRENTE                   CLIEV008                *
      *                                                                *
      *                                                                *
      *    MODULOS CHAMADOS.:                                          *
      *        BRAD7100 - TRATAR ERRO                                  *
      *        BRAD7600 - OBTER DATA DO SISTEMA                        *
      *                                                                *
      *================================================================*
PR2311*                        A L T E R A C A O                       *
'     *----------------------------------------------------------------*
'     * ANALISTA       :  PRISCILLA MARQUES - CAPGEMINI                *
'     * DATA           :  NOV/2015                                     *
'     * OBJETIVO       :  IMPLEMENTACAO DE CHECKPOINT/RESTART          *
'     *                   (RS 2.981.996)                               *
PR2311*================================================================*

      *===============================================================*
       ENVIRONMENT DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       CONFIGURATION SECTION.
      *---------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *---------------------------------------------------------------*
       INPUT-OUTPUT SECTION.
      *---------------------------------------------------------------*

       FILE-CONTROL.

           SELECT CONTASEL ASSIGN TO UT-S-CONTASEL
                      FILE STATUS IS WRK-FS-CONTASEL.

           SELECT CLIEADES ASSIGN TO UT-S-CLIEADES
                      FILE STATUS IS WRK-FS-CLIEADES.

           SELECT ATLZSIGB ASSIGN TO UT-S-ATLZSIGB
                      FILE STATUS IS WRK-FS-ATLZSIGB.

           SELECT EXCESIGB ASSIGN TO UT-S-EXCESIGB
                      FILE STATUS IS WRK-FS-EXCESIGB.

      *===============================================================*
       DATA DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *   INPUT :     ORG. SEQUENCIAL   -   LRECL = 050               *
      *---------------------------------------------------------------*

       FD  CONTASEL
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-CONTASEL             PIC X(050).

      *---------------------------------------------------------------*
      *   OUTPUT:     ORG. SEQUENCIAL   -   LRECL = 040               *
      *---------------------------------------------------------------*

       FD  CLIEADES
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-CLIEADES             PIC X(040).

      *---------------------------------------------------------------*
      *   OUTPUT:     ORG. SEQUENCIAL   -   LRECL = 050               *
      *---------------------------------------------------------------*

       FD  ATLZSIGB
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-ATLZSIGB             PIC X(050).

      *---------------------------------------------------------------*
      *   OUTPUT:     ORG. SEQUENCIAL   -   LRECL = 050               *
      *---------------------------------------------------------------*

       FD  EXCESIGB
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-EXCESIGB             PIC X(050).


      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*

       01  FILLER  PIC X(32)  VALUE  '*  INICIO DA WORKING GFCT3335  *'.

       01  FILLER  PIC X(32)  VALUE  '*   AREAS AUXILIARES           *'.

       01  WRK-ENCONTROU               PIC  X(01)          VALUE SPACES.



       01  FILLER  PIC X(32)  VALUE  '*   AREAS DE FILE STATUS       *'.

       01  WRK-BATCH                   PIC X(08) VALUE 'BATCH   '.

       01  WRK-FS-CONTASEL             PIC X(02) VALUE SPACES.
       01  WRK-FS-CLIEADES             PIC X(02) VALUE SPACES.
       01  WRK-FS-ATLZSIGB             PIC X(02) VALUE SPACES.
       01  WRK-FS-EXCESIGB             PIC X(02) VALUE SPACES.

       01  WRK-OPERACAO                PIC X(13) VALUE SPACES.
       01  WRK-ABERTURA                PIC X(13) VALUE 'NA ABERTURA'.
       01  WRK-LEITURA                 PIC X(13) VALUE 'NA LEITURA'.
       01  WRK-GRAVACAO                PIC X(13) VALUE 'NA GRAVACAO'.
       01  WRK-FECHAMENTO              PIC X(13) VALUE 'NO FECHAMENTO'.

       01  FILLER  PIC X(32)  VALUE  '* MASCARAS DE EDICAO DE TOTAIS *'.

       01  FILLER.
           05  WRK-MASK-LIDOS-CONTASEL PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.
           05  WRK-MASK-LIDOS-CLIEADES PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.
           05  WRK-MASK-LIDOS-CLIEV004 PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.
           05  WRK-MASK-LIDOS-CLIEV007 PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.
           05  WRK-MASK-LIDOS-CLIEV008 PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.
           05  WRK-MASK-GRAVA-ATLZSIGB PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.
           05  WRK-MASK-GRAVA-EXCESIGB PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.

       01  FILLER  PIC X(32)  VALUE  '*CONTASEL / EXCESIGB / ATLZSIGB*'.

       COPY 'I#SIGB05'.

       01  FILLER  PIC X(32)  VALUE  '*   AREAS LAYOUT CLIEADES      *'.

       COPY 'I#GFCTRU'.

       01  FILLER  PIC X(32)  VALUE  '*   AREAS TRATAMENTO ERRO    *'.

       COPY 'I#BRAD7C'.

       01  WRK-TEXTO.
           05  FILLER                  PIC X(05) VALUE 'ERRO'.
           05  WRK-OPERACAO-TXT        PIC X(13) VALUE SPACES.
           05  FILLER                  PIC X(12) VALUE ' DO ARQUIVO '.
           05  WRK-ARQ-TXT             PIC X(08) VALUE SPACES.

PR2311 01  WRK-CKRS0100                PIC X(08) VALUE 'CKRS0100'.

PR2311 01  WRK-AREA-RESTART.
           03  WRK-ACU-DISPLAY         PIC S9(005) COMP-3 VALUE ZEROS.
           03  WRK-TAM-03-COM-S        PIC -9(003) VALUE ZEROS.
           03  FILLER REDEFINES WRK-TAM-03-COM-S.
               05  FILLER              PIC  X(001).
               05  WRK-TAM-03-SEM-S    PIC  9(003).

           03  WRK-TAM-05-COM-S        PIC -9(005) VALUE ZEROS.
           03  FILLER REDEFINES WRK-TAM-05-COM-S.
               05  FILLER              PIC  X(001).
               05  WRK-TAM-05-SEM-S    PIC  9(005).

           03  WRK-TAM-07-COM-S        PIC -9(007) VALUE ZEROS.
           03  FILLER REDEFINES WRK-TAM-07-COM-S.
               05  FILLER              PIC  X(001).
               05  WRK-TAM-07-SEM-S    PIC  9(007).

           03  WRK-TAM-09-COM-S        PIC -9(009) VALUE ZEROS.
           03  FILLER REDEFINES WRK-TAM-09-COM-S.
               05  FILLER              PIC  X(001).
               05  WRK-TAM-09-SEM-S    PIC  9(009).
           03  ACU-LIDOS-CONTASEL      PIC  9(009) COMP-3 VALUE ZEROS.
           03  ACU-LIDOS-CLIEADES      PIC  9(009) COMP-3 VALUE ZEROS.
           03  ACU-GRAVA-ATLZSIGB      PIC  9(009) COMP-3 VALUE ZEROS.
           03  ACU-GRAVA-EXCESIGB      PIC  9(009) COMP-3 VALUE ZEROS.
           03  ACU-SELEC-CLIEV004      PIC  9(009) COMP-3 VALUE ZEROS.
           03  ACU-SELEC-CLIEV007      PIC  9(009) COMP-3 VALUE ZEROS.
           03  ACU-SELEC-CLIEV008      PIC  9(009) COMP-3 VALUE ZEROS.
           03  WRK-CHV-CONTASEL.
               05  WRK-AGENC-SEL       PIC 9(005) VALUE ZEROS.
               05  WRK-CONTA-SEL       PIC 9(007) VALUE ZEROS.
           03  WRK-CHV-CLIEADES.
               05  WRK-AGENC-CLI       PIC 9(005) VALUE ZEROS.
               05  WRK-CONTA-CLI       PIC 9(007) VALUE ZEROS.

PR2311 COPY 'I#CKRS01'.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*  AREAS DB2                   *'.
      *---------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
       01  FILLER  PIC X(32)  VALUE  '*  FIM  DA WORKING   GFCT3335  *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*


      *===============================================================*
       PROCEDURE DIVISION.
      *===============================================================*

      *===============================================================*
       0000-PRINCIPAL SECTION.
      *---------------------------------------------------------------*I

PR2311     CALL 'CKRS1000'.

PR2311     PERFORM 0500-INICIALIZAR-CKRS.

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR
             UNTIL WRK-FS-CONTASEL EQUAL '10'.

           PERFORM  2310-GRAVAR-TRAILER.

           PERFORM 3000-FINALIZAR.

           STOP    RUN.

      *---------------------------------------------------------------*
       0000-99-FIM. EXIT.
      *---------------------------------------------------------------*

PR2311*---------------------------------------------------------------*
'      0500-INICIALIZAR-CKRS          SECTION.
'     *---------------------------------------------------------------*
'     *===> INICIALIZACAO NO MODULO CKRS0100
'
'          INITIALIZE CKRS01-INTERFACE
'                     WRK-AREA-RESTART.
'
'          MOVE SPACES                     TO CK01-PLAN.
'          MOVE 'DB2 '                     TO CK01-ID-DB2.
'          MOVE 'I'                        TO CK01-FUNCAO.
'          MOVE WRK-AREA-RESTART           TO CK01-AREA-RESTART.
'          MOVE LENGTH OF WRK-AREA-RESTART TO CK01-TAM-AREA-RESTART.
'
'          PERFORM 0510-CALL-CKRS0100.
'
'          IF CK01-STATUS               EQUAL 'REST'
'             MOVE CK01-AREA-RESTART(1:CK01-TAM-AREA-RESTART)
'                                         TO  WRK-AREA-RESTART
'             MOVE ACU-LIDOS-CONTASEL     TO  WRK-MASK-LIDOS-CONTASEL
'             MOVE ACU-LIDOS-CLIEADES     TO  WRK-MASK-LIDOS-CLIEADES
'             MOVE ACU-SELEC-CLIEV004     TO  WRK-MASK-LIDOS-CLIEV004
'             MOVE ACU-SELEC-CLIEV007     TO  WRK-MASK-LIDOS-CLIEV007
'             MOVE ACU-SELEC-CLIEV008     TO  WRK-MASK-LIDOS-CLIEV008
'             MOVE ACU-GRAVA-ATLZSIGB     TO  WRK-MASK-GRAVA-ATLZSIGB
'             MOVE ACU-GRAVA-EXCESIGB     TO  WRK-MASK-GRAVA-EXCESIGB
'             DISPLAY '*********************** GFCT3335 ****************
'     -                                                        '*******'
'             DISPLAY '*
'     -                                                        '      *'
'             DISPLAY '* QTDE DE REGISTROS LIDOS NO CONTASEL... : '
'                                         WRK-MASK-LIDOS-CONTASEL ' *'
'             DISPLAY '* QTDE DE REGISTROS LIDOS NO CLIEADES... : '
'                                         WRK-MASK-LIDOS-CLIEADES ' *'
'             DISPLAY '* QTDE DE LINHAS LIDAS NO CLIEV008...... : '
'                                         WRK-MASK-LIDOS-CLIEV008 ' *'
'             DISPLAY '* QTDE DE LINHAS LIDAS NO CLIEV007...... : '
'                                         WRK-MASK-LIDOS-CLIEV007 ' *'
'             DISPLAY '* QTDE DE LINHAS LIDAS NO CLIEV004...... : '
'                                         WRK-MASK-LIDOS-CLIEV004 ' *'
'             DISPLAY '* QTDE DE REGISTROS GRAVADOS NO ATLZSIGB : '
'                                         WRK-MASK-GRAVA-ATLZSIGB ' *'
'             DISPLAY '* QTDE DE REGISTROS GRAVADOS NO EXCESIGB : '
'                                         WRK-MASK-GRAVA-EXCESIGB ' *'
'             DISPLAY '*
'     -                                                        '      *'
'             DISPLAY '*********************** GFCT3335 ****************
'     -                                                        '*******'
'          END-IF.
'
'     *---------------------------------------------------------------*
'      0500-99-FIM.                     EXIT.
'     *---------------------------------------------------------------*
'
'     *----------------------------------------------------------------*
'      0510-CALL-CKRS0100            SECTION.
'     *----------------------------------------------------------------*
'
'          CALL WRK-CKRS0100           USING CKRS01-INTERFACE.
'
'          IF  CK01-CODIGO-RETORNO       NOT EQUAL ZEROS
'              DISPLAY  '************ CLLA0595 ***********'
'              DISPLAY  '*                               *'
'              DISPLAY  '*    ERRO NA ROTINA CKRS0100    *'
'              DISPLAY  '*                               *'
'              DISPLAY  '*    COD.RETORNO: ' CK01-CODIGO-RETORNO
'              DISPLAY  '*    COD.MENSAG.: ' CK01-CODIGO-MENSAGEM
'              DISPLAY  '*    DESCRICAO..: ' CK01-TEXTO-MENSAGEM
'              DISPLAY  '*                               *'
'              DISPLAY  '*********************************'
'              MOVE 'APL'              TO ERR-TIPO-ACESSO
'              MOVE 'ERRO CKRS0100'    TO ERR-TEXTO
'              PERFORM 9999-ROTINA-ERRO
'          END-IF.
'
'     *---------------------------------------------------------------*
'      0510-99-FIM.                    EXIT.
PR2311*---------------------------------------------------------------*

      *===============================================================*
       1000-INICIALIZAR SECTION.
      *---------------------------------------------------------------*

           OPEN    INPUT   CONTASEL
                           CLIEADES
                   OUTPUT  ATLZSIGB
                           EXCESIGB.

           MOVE    WRK-ABERTURA        TO  WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           PERFORM 1200-TESTAR-VAZIO.

      *---------------------------------------------------------------*
       1000-99-FIM. EXIT.
      *---------------------------------------------------------------*


      *===============================================================*
       1100-TESTAR-FILE-STATUS SECTION.
      *---------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-CONTASEL.

           PERFORM 1120-TESTAR-FS-CLIEADES.

           PERFORM 1130-TESTAR-FS-ATLZSIGB.

           PERFORM 1140-TESTAR-FS-EXCESIGB.

      *---------------------------------------------------------------*
       1100-99-FIM. EXIT.
      *---------------------------------------------------------------*


      *===============================================================*
       1110-TESTAR-FS-CONTASEL SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-CONTASEL NOT EQUAL '00'
               DISPLAY '************** GFCT3335 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*              CONTASEL             *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-CONTASEL
                                                  '         *'
               DISPLAY '*         PROGRAMA ENCERRADO        *'
               DISPLAY '************** GFCT3335 *************'
               MOVE WRK-OPERACAO       TO  WRK-OPERACAO-TXT
               MOVE 'CONTASEL'         TO  WRK-ARQ-TXT
               MOVE WRK-TEXTO          TO  ERR-TEXTO
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1110-99-FIM. EXIT.
      *---------------------------------------------------------------*


      *===============================================================*
       1120-TESTAR-FS-CLIEADES   SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-CLIEADES   NOT EQUAL '00'
               DISPLAY '************** GFCT3335 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*              CLIEADES             *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-CLIEADES
                                                  '         *'
               DISPLAY '*         PROGRAMA ENCERRADO        *'
               DISPLAY '************** GFCT3335 *************'
               MOVE WRK-OPERACAO       TO  WRK-OPERACAO-TXT
               MOVE 'CLIEADES'         TO  WRK-ARQ-TXT
               MOVE WRK-TEXTO          TO  ERR-TEXTO
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1120-99-FIM. EXIT.
      *---------------------------------------------------------------*


      *===============================================================*
       1130-TESTAR-FS-ATLZSIGB   SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-ATLZSIGB   NOT EQUAL '00'
               DISPLAY '************** GFCT3335 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*              ATLZSIGB             *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-ATLZSIGB
                                                  '         *'
               DISPLAY '*         PROGRAMA ENCERRADO        *'
               DISPLAY '************** GFCT3335 *************'
               MOVE WRK-OPERACAO       TO  WRK-OPERACAO-TXT
               MOVE 'ATLZSIGB'         TO  WRK-ARQ-TXT
               MOVE WRK-TEXTO          TO  ERR-TEXTO
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1130-99-FIM. EXIT.
      *---------------------------------------------------------------*


      *===============================================================*
       1140-TESTAR-FS-EXCESIGB   SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-EXCESIGB   NOT EQUAL '00'
               DISPLAY '************** GFCT3335 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*              EXCESIGB             *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-EXCESIGB
                                                  '         *'
               DISPLAY '*         PROGRAMA ENCERRADO        *'
               DISPLAY '************** GFCT3335 *************'
               MOVE WRK-OPERACAO       TO  WRK-OPERACAO-TXT
               MOVE 'EXCESIGB'         TO  WRK-ARQ-TXT
               MOVE WRK-TEXTO          TO  ERR-TEXTO
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1140-99-FIM. EXIT.
      *---------------------------------------------------------------*


      *===============================================================*
       1200-TESTAR-VAZIO SECTION.
      *---------------------------------------------------------------*

           PERFORM 2100-LER-CONTASEL.

           IF  ACU-LIDOS-CONTASEL      EQUAL   ZEROS
               DISPLAY '************* GFCT3335 *************'
               DISPLAY '*                                  *'
               DISPLAY '*   ARQUIVO CONTASEL ESTA VAZIO    *'
               DISPLAY '*                                  *'
               DISPLAY '*        PROGRAMA ENCERRADO        *'
               DISPLAY '*                                  *'
               DISPLAY '************* GFCT3335 *************'
           END-IF.

           PERFORM 2200-LER-CLIEADES.

           IF  ACU-LIDOS-CLIEADES      EQUAL   ZEROS
               DISPLAY '************* GFCT3335 *************'
               DISPLAY '*                                  *'
               DISPLAY '*   ARQUIVO CLIEADES ESTA VAZIO    *'
               DISPLAY '*                                  *'
               DISPLAY '************* GFCT3335 *************'
           END-IF.

      *---------------------------------------------------------------*
       1200-99-FIM. EXIT.
      *---------------------------------------------------------------*


      *===============================================================*
       2000-PROCESSAR SECTION.
      *---------------------------------------------------------------*

           IF  WRK-CHV-CONTASEL        EQUAL WRK-CHV-CLIEADES
               PERFORM 2300-GRAVAR-ATLZSIGB
PR2311         PERFORM 2900-FUNCAO-CHECKPOINT
               PERFORM 2100-LER-CONTASEL
               PERFORM 2200-LER-CLIEADES
           ELSE
               IF  WRK-CHV-CONTASEL    GREATER WRK-CHV-CLIEADES
                   PERFORM 2200-LER-CLIEADES
               ELSE
                   PERFORM 2400-VERIFICAR-CLIE
                   IF  WRK-ENCONTROU   EQUAL 'S'
                       PERFORM 2300-GRAVAR-ATLZSIGB
                   END-IF

                   IF  WRK-ENCONTROU   EQUAL 'N'
                       PERFORM 2500-GRAVAR-EXCESIGB
                       ADD 1           TO WRK-ACU-DISPLAY
                   END-IF
                   PERFORM 2100-LER-CONTASEL
               END-IF
           END-IF.

      *---------------------------------------------------------------*
       2000-99-FIM. EXIT.
      *---------------------------------------------------------------*


      *===============================================================*
       2100-LER-CONTASEL SECTION.
      *---------------------------------------------------------------*

           READ CONTASEL INTO SIGB-ARQ-ALIMENTADOR.

           IF  WRK-FS-CONTASEL      EQUAL  '10'
               MOVE HIGH-VALUES        TO  WRK-CHV-CONTASEL
               GO                      TO  2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO  WRK-OPERACAO.
           PERFORM 1110-TESTAR-FS-CONTASEL.

           ADD  1                      TO  ACU-LIDOS-CONTASEL.

           MOVE SIGB-AGENCIA           TO  WRK-TAM-05-COM-S.
           MOVE WRK-TAM-05-SEM-S       TO  WRK-AGENC-SEL.
           MOVE SIGB-CONTA             TO  WRK-TAM-07-COM-S.
           MOVE WRK-TAM-07-SEM-S       TO  WRK-CONTA-SEL.

      *---------------------------------------------------------------*
       2100-99-FIM. EXIT.
      *---------------------------------------------------------------*


      *===============================================================*
       2200-LER-CLIEADES SECTION.
      *---------------------------------------------------------------*

           READ CLIEADES INTO GFCTRU-ENTRADA.

           IF  WRK-FS-CLIEADES      EQUAL  '10'
               MOVE HIGH-VALUES        TO  WRK-CHV-CLIEADES
               GO                      TO  2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO  WRK-OPERACAO.
           PERFORM 1120-TESTAR-FS-CLIEADES.

           ADD  1                      TO  ACU-LIDOS-CLIEADES.

           MOVE GFCTRU-AGENCIA         TO  WRK-TAM-05-COM-S.
           MOVE WRK-TAM-05-SEM-S       TO  WRK-AGENC-CLI.
           MOVE GFCTRU-CONTA           TO  WRK-TAM-07-COM-S.
           MOVE WRK-TAM-07-SEM-S       TO  WRK-CONTA-CLI.

           MOVE GFCTRU-CGC-CPF         TO  WRK-TAM-09-COM-S.
           MOVE GFCTRU-FILIAL          TO  WRK-TAM-05-COM-S.
           MOVE GFCTRU-CONTR           TO  WRK-TAM-03-COM-S.

      *---------------------------------------------------------------*
       2200-99-FIM. EXIT.
      *---------------------------------------------------------------*


      *===============================================================*
       2300-GRAVAR-ATLZSIGB  SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-TAM-09-SEM-S       TO  SIGB-CGCCPF-PRINC.
           MOVE WRK-TAM-05-SEM-S       TO  SIGB-CGCCPF-FILIAL.
           MOVE WRK-TAM-03-SEM-S       TO  SIGB-CGCCPF-DAC.

           WRITE FD-REG-ATLZSIGB     FROM  SIGB-ARQ-ALIMENTADOR.

           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO.
           PERFORM 1130-TESTAR-FS-ATLZSIGB.

           ADD 1                       TO  ACU-GRAVA-ATLZSIGB.

      *---------------------------------------------------------------*
       2300-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
       2310-GRAVAR-TRAILER   SECTION.
      *---------------------------------------------------------------*

           MOVE 99999                  TO  SIGB-AGENCIA.
           MOVE 9999999                TO  SIGB-CONTA.
           MOVE 999999999              TO  SIGB-CGCCPF-PRINC.
           MOVE 99999                  TO  SIGB-CGCCPF-FILIAL.
           MOVE 999                    TO  SIGB-CGCCPF-DAC.
           MOVE SPACES                 TO  SIGB-CCUSTO.
           MOVE ZEROS                  TO  SIGB-COD-CART.
           MOVE ZEROS                  TO  SIGB-DATAREF-CART.
           MOVE ACU-GRAVA-ATLZSIGB     TO  SIGB-DADO-CONS1.

           WRITE FD-REG-ATLZSIGB     FROM  SIGB-ARQ-ALIMENTADOR.

           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO.
           PERFORM 1130-TESTAR-FS-ATLZSIGB.

           ADD 1                       TO  ACU-GRAVA-ATLZSIGB.

      *---------------------------------------------------------------*
       2310-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
       2400-VERIFICAR-CLIE  SECTION.
      *---------------------------------------------------------------*

           MOVE 'N'                    TO WRK-ENCONTROU.

           PERFORM 2410-ACESSAR-CLIEV008.

           IF  SQLCODE EQUAL +100
               PERFORM 2420-ACESSAR-CLIEV007

               IF  SQLCODE EQUAL +100
                   GO                  TO 2400-99-FIM
               END-IF
           END-IF.

           PERFORM 2430-ACESSAR-CLIEV004.

           IF  SQLCODE EQUAL ZEROS
               MOVE   'S'              TO WRK-ENCONTROU
           END-IF.

      *---------------------------------------------------------------*
       2400-99-FIM. EXIT.
      *---------------------------------------------------------------*


      *===============================================================*
       2410-ACESSAR-CLIEV008 SECTION.
      *---------------------------------------------------------------*

           MOVE SIGB-AGENCIA           TO CJUNC-DEPDC      OF CLIEV008.
           MOVE SIGB-CONTA             TO CCTA-CLI         OF CLIEV008.

           EXEC  SQL
                 SELECT CID_CLI
                 INTO  :CLIEV008.CID-CLI
                 FROM   DB2PRD.V01CTA_CORRENTE
                 WHERE  CJUNC_DEPDC = :CLIEV008.CJUNC-DEPDC AND
                        CCTA_CLI    = :CLIEV008.CCTA-CLI
           END-EXEC

           IF ( SQLCODE NOT EQUAL ZEROS AND +100 ) OR
              ( SQLWARN0    EQUAL 'W' )
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'V01CTA_CORRENTE' TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE  SQLCODE          TO ERR-SQL-CODE
                MOVE '0001'            TO ERR-LOCAL
                MOVE  SPACES           TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE EQUAL ZEROS
               ADD  1                  TO ACU-SELEC-CLIEV008
           END-IF.

      *---------------------------------------------------------------*
       2410-99-FIM. EXIT.
      *---------------------------------------------------------------*


      *===============================================================*
       2420-ACESSAR-CLIEV007 SECTION.
      *---------------------------------------------------------------*

           MOVE SIGB-AGENCIA           TO CJUNC-DEPDC      OF CLIEV007.
           MOVE SIGB-CONTA             TO CCTA-CLI         OF CLIEV007.

           EXEC  SQL
                 SELECT CID_CLI
                 INTO  :CLIEV007.CID-CLI
                 FROM   DB2PRD.V01CTA_POUPANCA
                 WHERE  CJUNC_DEPDC = :CLIEV007.CJUNC-DEPDC AND
                        CCTA_CLI    = :CLIEV007.CCTA-CLI
           END-EXEC

           IF ( SQLCODE NOT EQUAL ZEROS AND +100 ) OR
              ( SQLWARN0    EQUAL 'W' )
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'V01CTA_POUPANCA' TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE  SQLCODE          TO ERR-SQL-CODE
                MOVE '0002'            TO ERR-LOCAL
                MOVE  SPACES           TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE EQUAL ZEROS
               MOVE CID-CLI            OF CLIEV007
                 TO CID-CLI            OF CLIEV008
               ADD  1                  TO ACU-SELEC-CLIEV007
           END-IF.

      *---------------------------------------------------------------*
       2420-99-FIM. EXIT.
      *---------------------------------------------------------------*


      *===============================================================*
       2430-ACESSAR-CLIEV004 SECTION.
      *---------------------------------------------------------------*

           MOVE CID-CLI OF CLIEV008    TO CID-CLI          OF CLIEV004.

           EXEC  SQL
                SELECT   CCGC_CPF,
                         CFLIAL_CGC,
                         CCTRL_CPF_CGC
                  INTO  :CLIEV004.CCGC-CPF,
                        :CLIEV004.CFLIAL-CGC,
                        :CLIEV004.CCTRL-CPF-CGC
                  FROM   DB2PRD.V01CLIENTE_AGENCIA
                  WHERE  CID_CLI     =   :CLIEV004.CID-CLI
           END-EXEC.

           IF ( SQLCODE NOT EQUAL ZEROS AND +100 ) OR
              ( SQLWARN0    EQUAL 'W' )
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'V01CLIENTE_AGENCIA'
                                       TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE  SQLCODE          TO ERR-SQL-CODE
                MOVE '0003'            TO ERR-LOCAL
                MOVE  SPACES           TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE EQUAL ZEROS
               MOVE CCGC-CPF           OF  CLIEV004
                                       TO  WRK-TAM-09-COM-S
               MOVE CFLIAL-CGC         OF  CLIEV004
                                       TO  WRK-TAM-05-COM-S
               MOVE CCTRL-CPF-CGC      OF  CLIEV004
                                       TO  WRK-TAM-03-COM-S
               ADD  1                  TO  ACU-SELEC-CLIEV004
           END-IF.

      *---------------------------------------------------------------*
       2430-99-FIM. EXIT.
      *---------------------------------------------------------------*


      *===============================================================*
       2500-GRAVAR-EXCESIGB  SECTION.
      *---------------------------------------------------------------*

           WRITE FD-REG-EXCESIGB     FROM  SIGB-ARQ-ALIMENTADOR.

           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO.
           PERFORM 1140-TESTAR-FS-EXCESIGB.

           ADD 1                       TO  ACU-GRAVA-EXCESIGB.

      *---------------------------------------------------------------*
       2500-99-FIM. EXIT.
      *---------------------------------------------------------------*

PR2311*---------------------------------------------------------------*
'      2900-FUNCAO-CHECKPOINT      SECTION.
'     *---------------------------------------------------------------*
'     *===> EFETUAR CHECKPOINT (COMMIT) NO MODULO CKRS0100
'
'          INITIALIZE CKRS01-INTERFACE.
'
'          MOVE 'DB2 '                     TO CK01-ID-DB2.
'          MOVE 'P'                        TO CK01-FUNCAO.
'
'          MOVE WRK-AREA-RESTART           TO CK01-AREA-RESTART.
'          MOVE LENGTH OF WRK-AREA-RESTART TO CK01-TAM-AREA-RESTART.
'
'          PERFORM 0510-CALL-CKRS0100.
'
'     *---------------------------------------------------------------*
'      2900-99-FIM.                    EXIT.
PR2311*---------------------------------------------------------------*

      *===============================================================*
       3000-FINALIZAR      SECTION.
      *---------------------------------------------------------------*

           CLOSE   CONTASEL
                   CLIEADES
                   ATLZSIGB
                   EXCESIGB.

           MOVE    WRK-FECHAMENTO      TO   WRK-OPERACAO.
           PERFORM 1100-TESTAR-FILE-STATUS.

           MOVE ACU-LIDOS-CONTASEL     TO  WRK-MASK-LIDOS-CONTASEL.
           MOVE ACU-LIDOS-CLIEADES     TO  WRK-MASK-LIDOS-CLIEADES.
           MOVE ACU-SELEC-CLIEV004     TO  WRK-MASK-LIDOS-CLIEV004.
           MOVE ACU-SELEC-CLIEV007     TO  WRK-MASK-LIDOS-CLIEV007.
           MOVE ACU-SELEC-CLIEV008     TO  WRK-MASK-LIDOS-CLIEV008.
           MOVE ACU-GRAVA-ATLZSIGB     TO  WRK-MASK-GRAVA-ATLZSIGB.
           MOVE ACU-GRAVA-EXCESIGB     TO  WRK-MASK-GRAVA-EXCESIGB.

           DISPLAY '*********************** GFCT3335 *******************
      -                                                        '****'.
           DISPLAY '*
      -                                                        '   *'.
           DISPLAY '* QTDE DE REGISTROS LIDOS NO CONTASEL... : '
                                       WRK-MASK-LIDOS-CONTASEL ' *'.
           DISPLAY '* QTDE DE REGISTROS LIDOS NO CLIEADES... : '
                                       WRK-MASK-LIDOS-CLIEADES ' *'.
           DISPLAY '* QTDE DE LINHAS LIDAS NO CLIEV008...... : '
                                       WRK-MASK-LIDOS-CLIEV008 ' *'.
           DISPLAY '* QTDE DE LINHAS LIDAS NO CLIEV007...... : '
                                       WRK-MASK-LIDOS-CLIEV007 ' *'.
           DISPLAY '* QTDE DE LINHAS LIDAS NO CLIEV004...... : '
                                       WRK-MASK-LIDOS-CLIEV004 ' *'.
           DISPLAY '* QTDE DE REGISTROS GRAVADOS NO ATLZSIGB : '
                                       WRK-MASK-GRAVA-ATLZSIGB ' *'.
           DISPLAY '* QTDE DE REGISTROS GRAVADOS NO EXCESIGB : '
                                       WRK-MASK-GRAVA-EXCESIGB ' *'.
           DISPLAY '*
      -                                                        '   *'.
           DISPLAY '*********************** GFCT3335 *******************
      -                                                        '****'.

PR2311     PERFORM 3500-FINALIZAR-CKRS.

      *---------------------------------------------------------------*
       3000-99-FIM. EXIT.
      *---------------------------------------------------------------*

PR2311*---------------------------------------------------------------*
'      3500-FINALIZAR-CKRS            SECTION.
'     *---------------------------------------------------------------*
'     *===> FINALIZACAO NO MODULO CKRS0100
'
'          INITIALIZE CKRS01-INTERFACE.
'
'          MOVE 'DB2'                  TO CK01-ID-DB2.
'          MOVE 'F'                    TO CK01-FUNCAO.
'
'          PERFORM 0510-CALL-CKRS0100.
'
'     *---------------------------------------------------------------*
'      3500-99-FIM.                    EXIT.
PR2311*---------------------------------------------------------------*

      *===============================================================*
       9999-ROTINA-ERRO SECTION.
      *---------------------------------------------------------------*

           MOVE 'GFCT3335'             TO  ERR-PGM

           CALL 'BRAD7100'          USING  WRK-BATCH
                                           ERRO-AREA
                                           SQLCA.

           GOBACK.

      *---------------------------------------------------------------*
       9999-99-FIM. EXIT.
      *---------------------------------------------------------------*
