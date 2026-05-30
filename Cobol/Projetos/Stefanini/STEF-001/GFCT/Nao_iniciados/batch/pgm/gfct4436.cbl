      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT4436.
       AUTHOR.     HOMI - SONDA.
      *
      *================================================================*
      *                 S O N D A - P R O C W O R K                    *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  GFCT4436                                     *
      *    SISTEMA.....:  GFCT - GESTAO E COBRANCA DE TARIFAS          *
      *    PROGRAMADOR.:  HOMI - SONDA                                 *
      *    ANALISTA....:  PAGNOCCA - SONDA                             *
      *    GRUPO.......:  45                                           *
      *    DATA........:  JUN / 2013                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  BALANCE LINE ENTRE:                          *
      *                   CADUFONI (ARQUIVO CADU COM OS FONES)         *
      *                   CADUFONO (ARQUIVO DE TELEFONE/RAMAL DO CLIE) *
      *                   GERAR ARQUIVO FORMATADO COM AS INFORMACOES   *
      *                   DE TELEFONE/RAMAL DO CLIENTE VIA CADU        *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME           I/O        INCLUDE/BOOK        *
      *                CADUFONI          I         GFCTWBUG (PSDCW018) *
      *                CADUFONO          O         GFCTWBUF            *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    GFCTWBUG - BOOK DE ENTRADA - ARQUIVO COMPRIMIDO (TREG=003)  *
      *    GFCTWBUF - BOOK DE SAIDA   - ARQUIVO DE TEL/RMAL VIA CADU   *
      *    I#FRWKGE - GRAVAR LOG DE ERRO P/ BATCH                      *
      *    I#FRWKAR - BOOK PARA TRATAMENTO DE ERROS DE ARQUIVOS        *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    FRWK2999 - GRAVAR LOG DE ERRO P/ BATCH                      *
      *    POOL0300 - LER ARQUIVO COMPRIMIDO                           *
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

           SELECT CADUFONO ASSIGN      TO   UT-S-CADUFONO
                      FILE STATUS      IS   WRK-FS-CADUFONO.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *   OUTPUT:ARQUIVO DE SAIDA    - ARQUIVO DE TEL/RMAL VIA CADU    *
      *          ORG. SEQUENCIAL     -   LRECL  = 036                  *
      *----------------------------------------------------------------*
       FD  CADUFONO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-CADUFONO             PIC  X(036).

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
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA AUXILIARES                            ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-AREA-AUXILIARES.
           03 WRK-FIM                  PIC  X(001)         VALUE 'N'.
           03 WRK-AUX-CCLUB-ANT        PIC S9(010)         VALUE ZEROS.
           03 WRK-AUX-CADUFONI         PIC  X(008)         VALUE
                                                           'CADUFONI'.
           03 WRK-AUX-TP-REG           PIC  X(003)         VALUE SPACES.
           03 FILLER  REDEFINES        WRK-AUX-TP-REG.
              05 FILLER                PIC  X(001).
              05 WRK-AUX-TREG          PIC  X(002).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE USO DO HEXA                        ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-S10                     PIC +9(010)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S10.
           05 FILLER                   PIC  X(001).
           05 WRK-S10-P-10             PIC  9(010).

       01  WRK-S11                     PIC +9(011)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S11.
           05 FILLER                   PIC  X(001).
           05 WRK-S11-P-11             PIC  9(011).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACUMULADORES                       ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-ACUMULADORES.
           03 WRK-LIDOS-CADUFONI       PIC  9(009) COMP-3  VALUE ZEROS.
           03 WRK-ACEITO-CADUFONI      PIC  9(009) COMP-3  VALUE ZEROS.
           03 WRK-GRAVADOS-CADUFONO    PIC  9(009) COMP-3  VALUE ZEROS.
           03 WRK-DISPLAY              PIC +Z(017)9        VALUE ZEROS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE TESTE DE FILE-STATUS               ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-FS-CADUFONO             PIC  X(002)         VALUE SPACES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ENTRADA CADUFONI (COMPRIMIDO)      ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       COPY GFCTWBUG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE SAIDA CADUFONO                     ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       COPY GFCTWBUF.

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
             UNTIL WRK-FIM     EQUAL     'S'.

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

           OPEN OUTPUT  CADUFONO.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           MOVE '1100-TESTAR-FILE-STATUS' TO   FRWKGHEA-IDEN-PARAGRAFO.

           PERFORM 1110-TESTAR-FS-CADUFONO.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-TESTAR-FS-CADUFONO         SECTION.
      *----------------------------------------------------------------*

           MOVE '1110-TESTAR-FS-CADUFONO' TO   FRWKGHEA-IDEN-PARAGRAFO.

           IF  WRK-FS-CADUFONO         NOT EQUAL  '00'
               MOVE 'CADUFONO'         TO FRWKGARQ-NOME-ARQUIVO
               MOVE WRK-FS-CADUFONO    TO FRWKGARQ-FILE-STATUS
               PERFORM 9100-FORMATAR-ERRO-ARQUIVO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           MOVE '2000-VERIFICAR-VAZIO' TO   FRWKGHEA-IDEN-PARAGRAFO.

           PERFORM 2200-LER-CADUFONI.

           IF (RETURN-CODE             EQUAL     4)
               DISPLAY '************ GFCT4436 ************'
               DISPLAY '*                                *'
               DISPLAY '*     ARQUIVO CADUFONI VAZIO     *'
               DISPLAY '*       PROGRAMA ENCERRADO       *'
               DISPLAY '*                                *'
               DISPLAY '************ GFCT4436 ************'
               MOVE 'CADUFONI'        TO FRWKGARQ-NOME-ARQUIVO
               MOVE '04'              TO FRWKGARQ-FILE-STATUS
               PERFORM 9000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-LER-CADUFONI               SECTION.
      *----------------------------------------------------------------*

           MOVE '2200-LER-CADUFONI'    TO     FRWKGHEA-IDEN-PARAGRAFO.

           CALL 'POOL0300'             USING  WRK-AUX-CADUFONI
                                              BUG-REGISTRO.

           IF  RETURN-CODE             EQUAL  4
               MOVE   'S'              TO   WRK-FIM
               GO TO 2200-99-FIM
           END-IF.

           ADD  1  TO   WRK-LIDOS-CADUFONI.

           MOVE BUG-TP-REG             TO   WRK-AUX-TP-REG.

      *------------------( FILTRAR REGISTROS )-------------------------*

           IF  WRK-AUX-TREG NOT EQUAL  '03'
               GO  TO  2200-LER-CADUFONI
           END-IF.

           IF  BUG-CCLUB   EQUAL  ZEROS
               GO  TO  2200-LER-CADUFONI
           END-IF.

           IF  BUG-CCLUB   EQUAL  WRK-AUX-CCLUB-ANT
               GO  TO  2200-LER-CADUFONI
           ELSE
               MOVE BUG-CCLUB TO  WRK-AUX-CCLUB-ANT
           END-IF.

      *----------------------------------------------------------------*

           ADD  1  TO   WRK-ACEITO-CADUFONI.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE '3000-PROCESSAR' TO FRWKGHEA-IDEN-PARAGRAFO.

           IF      BUG-CTPO-LIN-TFONI EQUAL 001
                   PERFORM 3600-MOVER-DADOS
                   MOVE '3'     TO    BUF-TIPO
                   PERFORM 3500-GRAVAR-CADUFONO
           ELSE
             IF    BUG-CTPO-LIN-TFONI EQUAL 002
                   PERFORM 3600-MOVER-DADOS
                   MOVE '1'     TO    BUF-TIPO
                   PERFORM 3500-GRAVAR-CADUFONO
             ELSE
               IF  BUG-CTPO-LIN-TFONI EQUAL 003 AND
                   BUG-CTPO-FONE-CLI  NOT EQUAL 005
                   PERFORM 3600-MOVER-DADOS
                   MOVE '2'     TO    BUF-TIPO
                   PERFORM 3500-GRAVAR-CADUFONO
               END-IF
             END-IF
           END-IF.

           PERFORM 2200-LER-CADUFONI.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3500-GRAVAR-CADUFONO            SECTION.
      *----------------------------------------------------------------*

           MOVE '3500-GRAVAR-CADUFONO' TO   FRWKGHEA-IDEN-PARAGRAFO.

           WRITE FD-REG-CADUFONO       FROM BUF-REG.

           PERFORM 1110-TESTAR-FS-CADUFONO.

           ADD   1                     TO   WRK-GRAVADOS-CADUFONO.

      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3600-MOVER-DADOS                SECTION.
      *----------------------------------------------------------------*

           MOVE '3600-MOVER-DADOS'     TO   FRWKGHEA-IDEN-PARAGRAFO.

           MOVE  BUG-CCLUB             TO   WRK-S10.
           MOVE  WRK-S10-P-10          TO   BUF-CLUB.

           MOVE  BUG-NLIN-TFONI        TO   WRK-S11.
           MOVE  WRK-S11-P-11          TO   BUF-FONE.

           MOVE  BUG-CDDDI             TO   BUF-DDI.
           MOVE  BUG-CDDDD             TO   BUF-DDD.
           MOVE  BUG-CID-RMAL-BIP-PAGER
                                       TO   BUF-RAMAL.

      *----------------------------------------------------------------*
       3600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*
      *                                                                *
           MOVE '9000-FINALIZAR'       TO FRWKGHEA-IDEN-PARAGRAFO.
      *                                                                *
           DISPLAY '*********** GFCT4436 ***********'
           DISPLAY '*                              *'
           DISPLAY '*     RESUMO PROCESSAMENTO     *'
           DISPLAY '* ---------------------------- *'
           DISPLAY '*                              *'
           MOVE    WRK-LIDOS-CADUFONI  TO WRK-DISPLAY
           DISPLAY '* LIDOS    CADUFONI : ' WRK-DISPLAY
           DISPLAY '*                              *'
           MOVE    WRK-ACEITO-CADUFONI TO WRK-DISPLAY
           DISPLAY '* ACEITOS  CADUFONI : ' WRK-DISPLAY
           DISPLAY '*                              *'
           MOVE    WRK-GRAVADOS-CADUFONO  TO WRK-DISPLAY
           DISPLAY '* GRAVADOS CADUFONO : ' WRK-DISPLAY
           DISPLAY '*                              *'
           DISPLAY '*********** GFCT4436 ***********'

           CLOSE CADUFONO.

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

           MOVE 'GFCT4436'             TO FRWKGHEA-NOME-PROGRAMA.

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
