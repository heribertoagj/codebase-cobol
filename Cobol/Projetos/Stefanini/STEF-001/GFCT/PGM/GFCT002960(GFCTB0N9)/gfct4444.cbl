      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT4444.
       AUTHOR.     HOMI - SONDA.
      *
      *================================================================*
      *                 S O N D A - P R O C W O R K                    *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  GFCT4444                                     *
      *    SISTEMA.....:  GFCT - GESTAO E COBRANCA DE TARIFAS          *
      *    PROGRAMADOR.:  HOMI - SONDA                                 *
      *    ANALISTA....:  PAGNOCCA - SONDA                             *
      *    GRUPO.......:  45                                           *
      *    DATA........:  JUN / 2013                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  GERAR ARQUIVO DE LOAD DA TABELA GFCTB0N9     *
      *                   DB2PRD.TCLI_SEM_CESTA                        *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME           I/O        INCLUDE/BOOK        *
      *                ADMESCOR          I         GFCTWBD0            *
      *                IMAGB0N9          O         --------            *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    GFCTWBD0 - BOOK DE ENTRADA - MOVIMENTOS DE ADESOES GFCT     *
      *    I#FRWKGE - GRAVAR LOG DE ERRO P/ BATCH                      *
      *    I#FRWKAR - BOOK PARA TRATAMENTO DE ERROS DE ARQUIVOS        *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    FRWK2999 - GRAVAR LOG DE ERRO P/ BATCH                      *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS   COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT ADMESCOR ASSIGN      TO   UT-S-ADMESCOR
                      FILE STATUS      IS   WRK-FS-ADMESCOR.

           SELECT IMAGB0N9 ASSIGN      TO   UT-S-IMAGB0N9
                      FILE STATUS      IS   WRK-FS-IMAGB0N9.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *   INPUT:   ARQUIVO DE ENTRADA - MOVIMENTOS ADESOES GFTCT       *
      *            ORG. SEQUENCIAL    - LRECL   = 404                  *
      *----------------------------------------------------------------*
       FD  ADMESCOR
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-ADMESCOR             PIC  X(404).

      *----------------------------------------------------------------*
      *   OUTPUT:ARQUIVO DE SAIDA    - MOVIMENTOS GFCT COM GERENTE     *
      *          ORG. SEQUENCIAL     -   LRECL  = 204                  *
      *----------------------------------------------------------------*
       FD  IMAGB0N9
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-IMAGB0N9             PIC  X(204).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE TRABALHO                           ***'.
      *----------------------------------------------------------------*

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER                      PIC  X(050)        VALUE
           '*** INICIO DA WORKING-STORAGE SECTION         ****'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACUMULADORES                       ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-ACUMULADORES.
           03 WRK-LIDOS-ADMESCOR       PIC  9(009) COMP-3  VALUE ZEROS.
           03 WRK-GRAVADOS-IMAGB0N9    PIC  9(009) COMP-3  VALUE ZEROS.
           03 WRK-DISPLAY              PIC +Z(017)9        VALUE ZEROS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE TESTE DE FILE-STATUS               ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-FS-ADMESCOR             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-IMAGB0N9             PIC  X(002)         VALUE SPACES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE USO DO HEXA           ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-CPO-03                  PIC  9(003)         VALUE ZEROS.
       01  FILLER  REDEFINES           WRK-CPO-03.
           03  FILLER                  PIC  9(001).
           03  WRK-CPO-02              PIC  9(002).

       01  WRK-CPO-11                  PIC  9(011)         VALUE ZEROS.
       01  FILLER  REDEFINES           WRK-CPO-11.
           03  FILLER                  PIC  9(001).
           03  WRK-CPO-10              PIC  9(010).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ENTRADA ADMESCOR                   ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       COPY 'GFCTWBD0'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE SAIDA IMAGB0N9                     ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-IMAGB0N9.
           10 WRK-CAG-BCRIA            PIC S9(5)V  COMP-3 VALUE ZEROS.
           10 WRK-CCTA-BCRIA           PIC S9(13)V COMP-3 VALUE ZEROS.
           10 WRK-CFUNC-BDSCO          PIC S9(9)V  COMP-3 VALUE ZEROS.
           10 WRK-CSERVC-TARIF         PIC S9(5)V  COMP-3 VALUE ZEROS.
           10 WRK-CINDCD-VNCLO-FL      PIC S9(1)V  COMP-3 VALUE ZEROS.
           10 WRK-VRENTB               PIC S9(15)V9(2)  COMP-3
                                                          VALUE ZEROS.
           10 WRK-CCPF-CNPJ            PIC S9(9)V  COMP-3 VALUE ZEROS.
           10 WRK-CFLIAL-CPF-CNPJ      PIC S9(5)V  COMP-3 VALUE ZEROS.
           10 WRK-CFLIAL-CPF-CNPJ-N    PIC X(1)          VALUE SPACES.
           10 WRK-CCTRL-CPF-CNPJ       PIC S9(2)V  COMP-3 VALUE ZEROS.
           10 WRK-IPSSOA-COPLT         PIC X(70)          VALUE SPACES.
           10 WRK-CPSSOA               PIC S9(10)V COMP-3 VALUE ZEROS.
           10 WRK-CDDI                 PIC X(4)          VALUE SPACES.
           10 WRK-CDDI-N               PIC X(1)          VALUE SPACES.
           10 WRK-CDDD                 PIC X(4)          VALUE SPACES.
           10 WRK-CDDD-N               PIC X(1)          VALUE SPACES.
           10 WRK-NLIN-TFONI           PIC S9(11)V COMP-3 VALUE ZEROS.
           10 WRK-CID-RMAL             PIC X(15)          VALUE SPACES.
           10 WRK-CID-RMAL-N           PIC X(1)           VALUE SPACES.
           10 WRK-CINDCD-REST-CNTAT    PIC S9(1)V  COMP-3 VALUE ZEROS.
           10 WRK-DABERT-CTA           PIC X(10)          VALUE SPACES.
           10 WRK-VINVES               PIC S9(16)V9(2)  COMP-3
                                                          VALUE ZEROS.
           10 WRK-HINCL-REG            PIC X(26)          VALUE SPACES.
           10 WRK-DPROCM-ATUAL         PIC X(10)          VALUE SPACES.
ST2511     10 WRK-CCPF-CNPJ-ST         PIC X(9)           VALUE SPACES.
ST2511     10 WRK-CFLIAL-CPF-CNPJ-ST   PIC X(5)           VALUE SPACES.
ST2511     10 WRK-CCTRL-CPF-CNPJ-ST    PIC S9(2)V  COMP-3 VALUE ZEROS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO FRWK2999                           ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-FRWK2999                PIC  X(008)         VALUE
           'FRWK2999'.

       01  WRK-AREA-ERRO.
           COPY 'I#FRWKGE'.
           05 WRK-BLOCO-INFO-ERRO.
              10 WRK-CHAR-INFO-ERRO    PIC  X(001) OCCURS 0 TO 30000
                                       TIMES DEPENDING ON
                                       FRWKGHEA-TAM-DADOS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA P/ FORMATACAO DE ERRO DE ARQUIVO      ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-AREA-ERRO-ARQUIVO.
           COPY 'I#FRWKAR'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD0450                           ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-AREA-BRAD0450.
           05 WRK-0450-ABEND-BAT       PIC S9(004) COMP    VALUE +1111.
           05 WRK-0450-DUMP-BAT        PIC  X(001)         VALUE 'S'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** FIM DA WORKING-STORAGE SECTION             ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           MOVE '0000-ROTINA-PRINCIPAL'   TO   FRWKGHEA-IDEN-PARAGRAFO.

           PERFORM 1000-INICIAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR
             UNTIL WRK-FS-ADMESCOR   EQUAL    '10'.

           PERFORM 9000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           MOVE '1000-INICIAR'         TO FRWKGHEA-IDEN-PARAGRAFO.

           INITIALIZE FRWKGHEA-REGISTRO
                      FRWKGARQ-REGISTRO.

           OPEN INPUT   ADMESCOR
                OUTPUT  IMAGB0N9

           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           MOVE '1100-TESTAR-FILE-STATUS' TO   FRWKGHEA-IDEN-PARAGRAFO.

           PERFORM 1110-TESTAR-FS-ADMESCOR.

           PERFORM 1130-TESTAR-FS-IMAGB0N9.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-TESTAR-FS-ADMESCOR         SECTION.
      *----------------------------------------------------------------*

           MOVE '1110-TESTAR-FS-ADMESCOR' TO   FRWKGHEA-IDEN-PARAGRAFO.

           IF  WRK-FS-ADMESCOR         NOT EQUAL  '00'
               MOVE 'ADMESCOR'         TO FRWKGARQ-NOME-ARQUIVO
               MOVE WRK-FS-ADMESCOR    TO FRWKGARQ-FILE-STATUS
               PERFORM 9100-FORMATAR-ERRO-ARQUIVO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1130-TESTAR-FS-IMAGB0N9         SECTION.
      *----------------------------------------------------------------*

           MOVE '1130-TESTAR-FS-IMAGB0N9' TO   FRWKGHEA-IDEN-PARAGRAFO.

           IF  WRK-FS-IMAGB0N9         NOT EQUAL  '00'
               MOVE 'IMAGB0N9'         TO FRWKGARQ-NOME-ARQUIVO
               MOVE WRK-FS-IMAGB0N9    TO FRWKGARQ-FILE-STATUS
               PERFORM 9100-FORMATAR-ERRO-ARQUIVO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           MOVE '2000-VERIFICAR-VAZIO' TO   FRWKGHEA-IDEN-PARAGRAFO.

           PERFORM 2100-LER-ADMESCOR.

           IF (WRK-LIDOS-ADMESCOR      EQUAL     ZEROS)
               DISPLAY '************ GFCT4444 ************'
               DISPLAY '*                                *'
               DISPLAY '*     ARQUIVO ADMESCOR VAZIO     *'
               DISPLAY '*       PROGRAMA ENCERRADO       *'
               DISPLAY '*                                *'
               DISPLAY '************ GFCT4444 ************'
               MOVE 'ADMESCOR'        TO FRWKGARQ-NOME-ARQUIVO
               MOVE WRK-FS-ADMESCOR   TO FRWKGARQ-FILE-STATUS
               PERFORM 9000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-LER-ADMESCOR               SECTION.
      *----------------------------------------------------------------*

           MOVE '2100-LER-ADMESCOR'    TO   FRWKGHEA-IDEN-PARAGRAFO.

           READ ADMESCOR               INTO GFCTWBD0-REG.

           IF WRK-FS-ADMESCOR          EQUAL     '10'
               GO TO 2100-99-FIM
           END-IF.

           PERFORM 1110-TESTAR-FS-ADMESCOR.
           ADD  1  TO   WRK-LIDOS-ADMESCOR.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE '3000-PROCESSAR' TO FRWKGHEA-IDEN-PARAGRAFO.

           PERFORM 3500-GRAVAR-IMAGB0N9.

           PERFORM 2100-LER-ADMESCOR.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3500-GRAVAR-IMAGB0N9            SECTION.
      *----------------------------------------------------------------*

           MOVE '3500-GRAVAR-IMAGB0N9' TO   FRWKGHEA-IDEN-PARAGRAFO.

           MOVE  BD0-AG                TO   WRK-CAG-BCRIA.
           MOVE  BD0-CTA               TO   WRK-CCTA-BCRIA.
           MOVE  BD0-COD-GERENTE       TO   WRK-CFUNC-BDSCO.
           MOVE  BD0-CESTA-GFCT        TO   WRK-CSERVC-TARIF.
           IF    BD0-FOLHA             EQUAL    'S'
                 MOVE  1               TO   WRK-CINDCD-VNCLO-FL
           ELSE
                 MOVE  2               TO   WRK-CINDCD-VNCLO-FL
           END-IF.
           MOVE  BD0-VR-RENTAB         TO   WRK-VRENTB.
ST2511*    MOVE  BD0-CPF               TO   WRK-CCPF-CNPJ.
ST2511     MOVE  BD0-CPF               TO   WRK-CCPF-CNPJ-ST.
           MOVE  BD0-CTR               TO   WRK-CPO-03.
           MOVE  WRK-CPO-02            TO   WRK-CCTRL-CPF-CNPJ
                                            WRK-CCTRL-CPF-CNPJ-ST
                                               
ST2511*    IF    BD0-FIL               EQUAL    ZEROS
ST2511     IF    BD0-FIL               EQUAL    '0000'
ST2511     OR    BD0-FIL               EQUAL    SPACES
                 MOVE  ZEROS           TO   WRK-CFLIAL-CPF-CNPJ
ST2511           MOVE  SPACES          TO   WRK-CFLIAL-CPF-CNPJ-ST
                 MOVE  '?'             TO   WRK-CFLIAL-CPF-CNPJ-N
           ELSE
ST2511*         XMOVE  BD0-FIL         TO   WRK-CFLIAL-CPF-CNPJ
ST2511*         XMOVE  '0'             TO   WRK-CFLIAL-CPF-CNPJ-N
ST2511           MOVE  BD0-FIL         TO   WRK-CFLIAL-CPF-CNPJ-ST          
           END-IF.

           MOVE  BD0-NOME-CLI(1:70)    TO   WRK-IPSSOA-COPLT.
           MOVE  BD0-CLUB              TO   WRK-CPO-11.
           MOVE  WRK-CPO-10            TO   WRK-CPSSOA.
           IF    BD0-F-DDI             EQUAL    SPACES
                 MOVE  SPACES          TO   WRK-CDDI
                 MOVE  '?'             TO   WRK-CDDI-N
           ELSE
                 MOVE  BD0-F-DDI       TO   WRK-CDDI
                 MOVE  '0'             TO   WRK-CDDI-N
           END-IF.
           IF    BD0-F-DDD             EQUAL    SPACES
                 MOVE  SPACES          TO   WRK-CDDD
                 MOVE  '?'             TO   WRK-CDDD-N
           ELSE
                 MOVE  BD0-F-DDD       TO   WRK-CDDD
                 MOVE  '0'             TO   WRK-CDDD-N
           END-IF.
           MOVE  BD0-F-FONE            TO   WRK-NLIN-TFONI.
           IF    BD0-F-RAMAL           EQUAL    SPACES
                 MOVE  SPACES          TO   WRK-CID-RMAL
                 MOVE  '?'             TO   WRK-CID-RMAL-N
           ELSE
                 MOVE  BD0-F-RAMAL     TO   WRK-CID-RMAL
                 MOVE  '0'             TO   WRK-CID-RMAL-N
           END-IF.
           IF    BD0-PROCON            EQUAL    'S'
                 MOVE  1               TO   WRK-CINDCD-REST-CNTAT
           ELSE
                 MOVE  2               TO   WRK-CINDCD-REST-CNTAT
           END-IF.
           MOVE  BD0-DT-ABERT-CTA      TO   WRK-DABERT-CTA.
           MOVE  BD0-VR-INVEST         TO   WRK-VINVES.
           MOVE  BD0-TMS-GERACAO       TO   WRK-HINCL-REG.
           MOVE  BD0-DATA-GFCT         TO   WRK-DPROCM-ATUAL.

           WRITE FD-REG-IMAGB0N9       FROM WRK-IMAGB0N9.

           PERFORM 1130-TESTAR-FS-IMAGB0N9.

           ADD   1                     TO   WRK-GRAVADOS-IMAGB0N9.

      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*
      *                                                                *
           MOVE '9000-FINALIZAR'       TO FRWKGHEA-IDEN-PARAGRAFO.
      *                                                                *
           DISPLAY '*********** GFCT4444 ***********'
           DISPLAY '*                              *'
           DISPLAY '*     RESUMO PROCESSAMENTO     *'
           DISPLAY '* ---------------------------- *'
           MOVE    WRK-LIDOS-ADMESCOR  TO WRK-DISPLAY
           DISPLAY '* LIDOS    ADMESCOR : ' WRK-DISPLAY
           DISPLAY '*                              *'
           MOVE    WRK-GRAVADOS-IMAGB0N9  TO WRK-DISPLAY
           DISPLAY '* GRAVADOS IMAGB0N9 : ' WRK-DISPLAY
           DISPLAY '*                              *'
           DISPLAY '*********** GFCT4444 ***********'

           CLOSE ADMESCOR
                 IMAGB0N9.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9100-FORMATAR-ERRO-ARQUIVO      SECTION.
      *----------------------------------------------------------------*

           SET ERRO-ARQUIVO            TO TRUE.

           MOVE FRWKGARQ-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS.
           MOVE WRK-AREA-ERRO-ARQUIVO  TO WRK-BLOCO-INFO-ERRO
                                          (1:FRWKGHEA-TAM-DADOS).

           PERFORM 9900-TRATAR-ERRO.

      *----------------------------------------------------------------*
       9100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9900-TRATAR-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT4444'             TO FRWKGHEA-NOME-PROGRAMA.

           PERFORM 9990-GRAVAR-LOG-ERRO.

           PERFORM 9999-ABENDAR-PROGRAMA.

           GOBACK.

      *----------------------------------------------------------------*
       9900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9990-GRAVAR-LOG-ERRO            SECTION.
      *----------------------------------------------------------------*

           CALL WRK-FRWK2999           USING WRK-AREA-ERRO.

      *----------------------------------------------------------------*
       9990-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ABENDAR-PROGRAMA           SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*** BRAD0450 CHAMADO PARA ABENDAR O PROGRAMA ***'.
           DISPLAY ' '.

           CALL 'BRAD0450'             USING WRK-0450-ABEND-BAT
                                             WRK-0450-DUMP-BAT.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
