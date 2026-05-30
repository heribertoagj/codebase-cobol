      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. RCOR8010.
       AUTHOR.     PRIME INFORMATICA.
      *================================================================*
      *                 P R I M E   I N F O R M A T I C A              *
      *----------------------------------------------------------------*
      *    PROGRAMA....: RCOR8010                                      *
      *    PROGRAMADOR.: PRIME INFORMATICA        - PRIME              *
      *    ANALISTA....: PRIME INFORMATICA        - PRIME              *
      *    DATA........: AGO/2012                                      *
      *----------------------------------------------------------------*
      *    TRANSACAO...: M581                                          *
      *----------------------------------------------------------------*
      *    OBJETIVO....: RECEPCIONAR MSG COR0001E ENVIADA PELO BACEN,  *
      *                  GRAVAR AS INFORMACOES NAS BASES DO RCOR E     *
      *                  ENVIAR A MSG P/ O SISTEMA LEGADO DE ORIGEM.   *
      *----------------------------------------------------------------*
      *    BOOKS.......:                                               *
      *    I#RCOR11 - AREA DE COMUNICACAO COM MODULO RCOR6006          *
      *    I#POOLB6 - AREA DE TRATAMENTO DE ERRO - CDES                *
      *    I#RCOR15 - AREA DE COMUNICACAO TRIGGER DA MENSAGEM          *
      *    I#RCOR16 - AREA DE COMUNICACAO COM MODULO RCOR8015          *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    RCOR6006 - MODULO PARA EXECUCAO DOS COMANDO MQ              *
      *    RCOR8015 - MODULO NEGOCIO - TRATA BLOCO DE RETORNO          *
      *    CDES0111 - MODULO PARA GERACAO DO LOG DE ERRO               *
      *================================================================*
CG2605*                      A L T E R A C O E S                       *
CG2605*================================================================*
CG2605*    PROGRAMADOR.:  HERIBERTO ANTONIO GIANNASI JUNIOR            *
CG2605*    DATA........:  MAIO/2026                                     *
CG2605*----------------------------------------------------------------*
CG2605*    OBJETIVO....:  AGROLEG-1243 - MANTER FLUXO MQ LEGADO       *
CG2605*                   RCOR6006/SBAT3510; COMPACTACAO WKTPART       *
CG2605*                   {2210} NO RCOR8015 APOS PARSE SIMM.         *
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
       77  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING RCOR8010  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC X(32)           VALUE
           '*          AUXILIARES          *'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC X(08)           VALUE SPACES.
       77  WRK-HANDLE                  PIC 9(08) COMP      VALUE ZEROS.
       77  WRK-DATA-DB2                PIC X(10)           VALUE SPACES.

       01  WRK-ERRO-CICS.
           03  FILLER                  PIC  X(08)          VALUE
               'LOCAL = '.
           03  WRK-LOCAL-ERRO          PIC  9(03)          VALUE ZEROS.
           03  FILLER                  PIC  X(01)          VALUE SPACES.
           03  FILLER                  PIC  X(10)          VALUE
               'EIBRESP = '.
           03  WRK-EIBRESP             PIC  9(04)          VALUE ZEROS.
           03  FILLER                  PIC  X(01)          VALUE SPACES.
           03  WRK-TEXTO-ERRO          PIC  X(52)          VALUE SPACES.

       01  WRK-ERRO-MODULO.
           03  FILLER                  PIC  X(08)          VALUE
               'LOCAL = '.
           03  WRK-LOCAL-ERRO-MOD      PIC  9(03)          VALUE ZEROS.
           03  FILLER                  PIC  X(01)          VALUE SPACES.
           03  FILLER                  PIC  X(14)          VALUE
               'COD RETORNO = '.
           03  WRK-COD-RETORNO         PIC  9(05)          VALUE ZEROS.
           03  FILLER                  PIC  X(01)          VALUE SPACES.
           03  WRK-TXT-ERRO-MOD        PIC  X(47)          VALUE SPACES.

       01  WRK-MSG-ERROS.
           03  WRK-MSG001              PIC  X(52)          VALUE
               'ERRO NO COMANDO RETRIVE PARA OBTER COMMAREA'.
           03  WRK-MSG002              PIC  X(47)          VALUE
               'ERRO ACESSO MODULO RCOR6006 - OPEN FILA MQ'.
           03  WRK-MSG003              PIC  X(47)          VALUE
               'ERRO ACESSO MODULO RCOR6006'.
           03  WRK-MSG004              PIC  X(47)          VALUE
               'ERRO ACESSO MODULO RCOR6006 - FETCH FILA MQ'.
           03  WRK-MSG005              PIC  X(47)          VALUE
               'ERRO ACESSO MODULO RCOR8015 - TRATA COR0001E'.
           03  WRK-MSG006              PIC  X(47)          VALUE
               'ERRO ACESSO MODULO RCOR6006 - CLOSE FILA MQ'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '* AREA DE COMUNICACAO RCOR6006 *'.
      *----------------------------------------------------------------*

       COPY 'I#RCOR11'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '* AREA DE COMUNICACAO RCOR8015 *'.
      *----------------------------------------------------------------*

       COPY 'I#RCOR16'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREAS PARA CDES0111      *'.
      *----------------------------------------------------------------*

       COPY 'I#POOLB6'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA DE COMUNICACAO       *'.
      *----------------------------------------------------------------*

       01  WRK-RCOR15.
           COPY 'I#RCOR15'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA DB2                  *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    FIM DA WORKING RCOR8010   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  DFHCOMMAREA                 PIC  X(32500).

      *================================================================*
       PROCEDURE                       DIVISION USING DFHCOMMAREA.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               SET :WRK-DATA-DB2 = CURRENT DATE
           END-EXEC.

           EXEC CICS HANDLE ABEND
                LABEL (9000-FORMATAR-ERRO-CICS)
           END-EXEC.

           IF (EIBRESP                 EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF.

           EXEC CICS
               RETRIEVE
               INTO   (WRK-RCOR15)
               LENGTH (LENGTH OF WRK-RCOR15)
               NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL ZEROS)
               MOVE WRK-MSG001         TO WRK-TEXTO-ERRO
               MOVE EIBRESP            TO WRK-EIBRESP
               MOVE 10                 TO WRK-LOCAL-ERRO
               PERFORM 9000-FORMATAR-ERRO-CICS
               PERFORM 2000-FINALIZAR
           END-IF.

           PERFORM 1000-PROCESSAR.

           PERFORM 2000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 1100-ABRIR-FILA.

           PERFORM 1200-LER-FILA.

           PERFORM 1300-TRATAR-MENSAGEM.

           PERFORM 1400-FECHAR-FILA.

           IF (RCOR16-S-CD-RETORNO     NOT EQUAL ZEROS)
               PERFORM 9300-ROTINA-ROLLBACK
           ELSE
               PERFORM 9400-ROTINA-SYNCPOINT
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-ABRIR-FILA                 SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO RCOR11-REGISTRO.

           INITIALIZE RCOR11-REGISTRO.

           MOVE 'O'                    TO RCOR11-A-FUNCAO.
           MOVE MQTMC-QNAME            TO RCOR11-A-FILA.

           PERFORM 1110-ACESSAR-RCOR6006.

           IF (RCOR11-S-CD-RETORNO     NOT EQUAL ZEROS)
               MOVE WRK-MSG002         TO WRK-TXT-ERRO-MOD
               MOVE 'RCOR6006'         TO WRK-MODULO
               MOVE RCOR11-S-CD-RETORNO
                                       TO WRK-COD-RETORNO
               MOVE 20                 TO WRK-LOCAL-ERRO-MOD
               PERFORM 9200-FORMATAR-ERRO-MODULO
               PERFORM 2000-FINALIZAR
           END-IF.

           MOVE RCOR11-A-HANDLE-FILA   TO WRK-HANDLE.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-ACESSAR-RCOR6006           SECTION.
      *----------------------------------------------------------------*

           EXEC CICS LINK
               PROGRAM ('RCOR6006')
               COMMAREA(RCOR11-REGISTRO)
               LENGTH  (LENGTH OF RCOR11-REGISTRO)
               NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL ZEROS)
               MOVE WRK-MSG003         TO WRK-TEXTO-ERRO
               MOVE EIBRESP            TO WRK-EIBRESP
               MOVE 30                 TO WRK-LOCAL-ERRO
               PERFORM 9000-FORMATAR-ERRO-CICS
               PERFORM 9300-ROTINA-ROLLBACK
               PERFORM 2000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-LER-FILA                   SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO RCOR11-REGISTRO.

           INITIALIZE RCOR11-REGISTRO.

           MOVE 'G'                    TO RCOR11-A-FUNCAO.
           MOVE MQTMC-QNAME            TO RCOR11-A-FILA.
           MOVE WRK-HANDLE             TO RCOR11-A-HANDLE-FILA.

           PERFORM 1110-ACESSAR-RCOR6006.

           IF (RCOR11-S-CD-RETORNO     NOT EQUAL ZEROS AND 100) OR
              (RCOR11-S-REASON-CODE    NOT EQUAL ZEROS AND 2033)
               MOVE WRK-MSG004         TO WRK-TXT-ERRO-MOD
               MOVE 'RCOR6006'         TO WRK-MODULO
               MOVE RCOR11-S-CD-RETORNO
                                       TO WRK-COD-RETORNO
               MOVE 40                 TO WRK-LOCAL-ERRO-MOD
               PERFORM 1400-FECHAR-FILA
               PERFORM 9300-ROTINA-ROLLBACK
               PERFORM 9200-FORMATAR-ERRO-MODULO
               PERFORM 2000-FINALIZAR
           END-IF.

           IF (RCOR11-S-CD-RETORNO     EQUAL 100)
               PERFORM 1400-FECHAR-FILA
               PERFORM 2000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1300-TRATAR-MENSAGEM            SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO RCOR16-REGISTRO.

           INITIALIZE RCOR16-REGISTRO.

           MOVE RCOR11-A-MENSAGEM      TO RCOR16-A-AREA-MSG.

           EXEC CICS LINK
               PROGRAM ('RCOR8015')
               COMMAREA(RCOR16-REGISTRO)
               LENGTH  (LENGTH OF RCOR16-REGISTRO)
               NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL ZEROS)
               MOVE 99                 TO RCOR16-S-CD-RETORNO
               MOVE WRK-MSG005         TO WRK-TEXTO-ERRO
               MOVE EIBRESP            TO WRK-EIBRESP
               MOVE 50                 TO WRK-LOCAL-ERRO
               PERFORM 9000-FORMATAR-ERRO-CICS
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1400-FECHAR-FILA                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO RCOR11-REGISTRO.

           INITIALIZE RCOR11-REGISTRO.

           MOVE 'C'                    TO RCOR11-A-FUNCAO.
           MOVE MQTMC-QNAME            TO RCOR11-A-FILA.
           MOVE WRK-HANDLE             TO RCOR11-A-HANDLE-FILA.

           PERFORM 1110-ACESSAR-RCOR6006.

           IF (RCOR11-S-CD-RETORNO     NOT EQUAL ZEROS)
               MOVE WRK-MSG006         TO WRK-TXT-ERRO-MOD
               MOVE 'RCOR6006'         TO WRK-MODULO
               MOVE RCOR11-S-CD-RETORNO
                                       TO WRK-COD-RETORNO
               MOVE 60                 TO WRK-LOCAL-ERRO-MOD
               PERFORM 9200-FORMATAR-ERRO-MODULO
               PERFORM 9300-ROTINA-ROLLBACK
               PERFORM 2000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
               RETURN
           END-EXEC.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9000-FORMATAR-ERRO-CICS         SECTION.
      *----------------------------------------------------------------*

           MOVE 'VRS001'               TO ERR-VERSAO.
           MOVE ZEROS                  TO ERR-COD-MSG.
           MOVE WRK-ERRO-CICS          TO ERR-TEXTO-MSG.
           MOVE ZEROS                  TO ERR-RETURN-CODE.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.
           MOVE 'RCOR8010'             TO ERR-PGM.
           MOVE SPACES                 TO ERR-TEXTO.
           MOVE 'RCOR8010'             TO ERR-MODULO.
           MOVE DFHEIBLK               TO ERR-DFHEIBLK.

           EXEC CICS ASSIGN
                     APPLID(ERR-CICS)
           END-EXEC.

           PERFORM 9100-GRAVAR-LOG-CDES.

      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9100-GRAVAR-LOG-CDES            SECTION.
      *----------------------------------------------------------------*

           EXEC  CICS START
                 TRANSID('CD11')
                 FROM   (ERRO-AREA)
                 LENGTH (+520)
                 NOHANDLE
           END-EXEC.

           IF (EIBRESP                 EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF.

      *----------------------------------------------------------------*
       9100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9200-FORMATAR-ERRO-MODULO       SECTION.
      *----------------------------------------------------------------*

           MOVE 'VRS001'               TO ERR-VERSAO.
           MOVE ZEROS                  TO ERR-COD-MSG.
           MOVE WRK-ERRO-MODULO        TO ERR-TEXTO-MSG.
           MOVE ZEROS                  TO ERR-RETURN-CODE.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.
           MOVE 'RCOR8010'             TO ERR-PGM.
           MOVE SPACES                 TO ERR-TEXTO.
           MOVE WRK-MODULO             TO ERR-MODULO.
           MOVE DFHEIBLK               TO ERR-DFHEIBLK.

           EXEC CICS ASSIGN
                     APPLID(ERR-CICS)
           END-EXEC.

           PERFORM 9100-GRAVAR-LOG-CDES.

      *----------------------------------------------------------------*
       9200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9300-ROTINA-ROLLBACK            SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
               SYNCPOINT ROLLBACK
           END-EXEC.

           IF (EIBRESP                 EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF.

      *----------------------------------------------------------------*
       9300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9400-ROTINA-SYNCPOINT           SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
               SYNCPOINT
           END-EXEC.

           IF (EIBRESP                 EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF.

      *----------------------------------------------------------------*
       9400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
