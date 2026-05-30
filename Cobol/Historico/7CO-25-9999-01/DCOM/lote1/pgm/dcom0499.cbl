      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM0499.
       AUTHOR.     LUCIANDRA SILVEIRA.
      *================================================================*
      *                    D T S   C O N S U L T I N G                 *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM0499                                    *
      *    PROGR/ANALIS:   LUCIANDRA SILVEIRA - DTS CONSULTING         *
      *    ANALISTA DDS:   EDGARD R. DE SOUZA - BRADESCO - GRUPO 70    *
      *    DATA........:   18/03/2011                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   EFETUAR O COMMIT DAS TRANSACOES CHAMADORAS. *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *         POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC.*
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *    CHAMADO POR.:   DCOM0403                                    *
      *                    DCOM0404                                    *
      *                    DCOM0406                                    *
      *                    DCOM0407                                    *
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
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM0499 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES PARA POOL5005 ***'.
      *----------------------------------------------------------------*

       01  WRK-POOL5005.
           05 WRK-GU                   PIC  X(004)         VALUE 'GU  '.
           05 WRK-CHNG                 PIC  X(004)         VALUE 'CHNG'.
           05 WRK-ISRT                 PIC  X(004)         VALUE 'ISRT'.
           05 WRK-FUNCAO               PIC  X(004)         VALUE SPACES.
           05 WRK-TELA                 PIC  X(008)         VALUE SPACES.
           05 WRK-VERSAO               PIC  X(006)         VALUE
              'VRS002'.

           05 WRK-COD-USER             PIC  X(007)         VALUE SPACES.
           05 WRK-COD-USER-R           REDEFINES
              WRK-COD-USER             PIC  9(007).

           05 WRK-COD-DEPTO.
              10 FILLER                PIC  X(002)         VALUE SPACES.
              10 WRK-COD-DEPTO-N       PIC  9(004)         VALUE ZEROS.

           05 WRK-MENSAGEM.
              10 FILLER                PIC  X(004)         VALUE SPACES.
              10 WRK-TRANSACAO         PIC  X(008)         VALUE SPACES.
              10 FILLER                PIC  X(1988)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO ***'.
      *----------------------------------------------------------------*

       01  WRK-COMU-AREA.
           05 WRK-COMU-LL              PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-COMU-ZZ              PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-COMU-TRANCODE.
              10 WRK-COMU-TRANSACAO    PIC  X(008)         VALUE SPACES.
              10 FILLER                PIC  X(006)         VALUE SPACES.
              10 WRK-COMU-PFK          PIC  X(001)         VALUE SPACES.
           05 WRK-COMU-SENHAS          PIC  X(037)         VALUE SPACES.
           05 WRK-COMU-COMANDO         PIC  X(068)         VALUE SPACES.
           05 WRK-COMU-DADOS.
              10 WRK-COMU-OPCAO        PIC  9(001)         VALUE ZEROS.
              10 WRK-COMU-OPER         PIC  9(013)         VALUE ZEROS.
              10 WRK-COMU-AGENCIA-1    PIC  9(005)         VALUE ZEROS.
              10 WRK-COMU-AGENCIA-2    PIC  9(005)         VALUE ZEROS.
              10 WRK-COMU-CONTA        PIC  9(013)         VALUE ZEROS.
FS2511*       10 WRK-COMU-CNPJ         PIC  9(009)         VALUE ZEROS.
FS2511        10 WRK-COMU-CNPJ         PIC  X(009)         VALUE SPACES.
FS2511*       10 WRK-COMU-FILIAL       PIC  9(004)         VALUE ZEROS.
FS2511        10 WRK-COMU-FILIAL       PIC  X(04)          VALUE SPACES.
              10 WRK-COMU-DIG          PIC  9(002)         VALUE ZEROS.
              10 WRK-COMU-OPERACAO.
                 15 WRK-COMU-DANO-OPER PIC  9(004)         VALUE ZEROS.
                 15 WRK-COMU-NSEQ-OPER PIC  9(009)         VALUE ZEROS.
              10 WRK-COMU-TIMESTAMP    PIC  X(026)         VALUE ZEROS.
              10 WRK-COMU-MENSAGEM     PIC  X(079)         VALUE SPACES.
              10 WRK-COMU-PAGINA       PIC  9(003)         VALUE ZEROS.
              10 WRK-COMU-OCORR        PIC  9(002)         VALUE ZEROS.
              10 WRK-COMU-INPUT        PIC  X(1000)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM0499 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01 LNK-IO-PCB.
           05 LNK-IO-LTERM             PIC  X(008).
           05 FILLER                   PIC  X(002).
           05 LNK-IO-STATUS            PIC  X(002).
           05 FILLER                   PIC  X(012).
           05 LNK-IO-MODNAME           PIC  X(008).

       01 LNK-ALT-PCB.
           05 LNK-ALT-LTERM            PIC  X(008).
           05 FILLER                   PIC  X(002).
           05 LNK-ALT-STATUS           PIC  X(002).
           05 FILLER                   PIC  X(012).
           05 LNK-ALT-MODNAME          PIC  X(008).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY 'DLITCBL'             USING LNK-IO-PCB
                                             LNK-ALT-PCB.

           MOVE WRK-GU                 TO WRK-FUNCAO.

      *----------------------------------------------------------------*
      *  ESSA ROTINA CONTEM OS PROCEDIMENTOS INICIAIS                  *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL5005'             USING WRK-VERSAO
                                             WRK-FUNCAO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             WRK-MENSAGEM
                                             WRK-TELA
                                             WRK-COD-USER
                                             WRK-COD-DEPTO.

           IF  RETURN-CODE             EQUAL 04 OR 12
               PERFORM 2000-FINALIZAR
           END-IF.

           PERFORM 1000-PROCESSAR-MENSAGEM.

           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PROGRAMA A MENSAGEM                               *
      *----------------------------------------------------------------*
       1000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM (1:LENGTH OF WRK-COMU-AREA)
                                       TO WRK-COMU-AREA.
           MOVE LENGTH                 OF WRK-COMU-AREA
                                       TO WRK-COMU-LL.
           MOVE ZEROS                  TO WRK-COMU-ZZ.

           MOVE 'DCOM0499'             TO WRK-COMU-TRANSACAO
           MOVE '3'                    TO WRK-COMU-PFK.
           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE WRK-TRANSACAO          TO WRK-TELA.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA FINALIZA O PROCESSAMENTO DO PROGRAMA              *
      *----------------------------------------------------------------*
       2000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
