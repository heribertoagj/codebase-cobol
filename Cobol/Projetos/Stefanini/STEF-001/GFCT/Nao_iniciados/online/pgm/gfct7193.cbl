      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT7193.
       AUTHOR.     RICARDO PEREIRA DA SILVA.
      *================================================================*
      *    S O N D A   P R O C W O R K  -  C O N S U L T O R I A       *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT7193                                    *
      *    PROGRAMADOR :   RICARDO PEREIRA DA SILVA - PROCWORK         *
      *    ANALISTA....:   RICARDO PEREIRA DA SILVA - PROCWORK         *
      *    DATA........:   16/05/2008                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   EFETUAR  COMUNICACAO  ENTRE  WEB  E  MF VIA *
      *      SBAT3000  PARA  AS  CHAMADAS  NOS  MODULOS  REFERENTES  A *
      *      FUNCIONALIDADE DE DEMONSTRATIVO DE UTILIZACAO DO PACOTE   *
      *----------------------------------------------------------------*
      *    INC'S ......:                                               *
      *    I#ISDDPC - AREA DE COMUNICACAO COM O ISD (SBAT3000)         *
      *    I#GFCT0K - AREA DE COMUNICACAO - SBAT3000 - ENTRADA         *
      *    I#GFCT0L - AREA DE COMUNICACAO - SBAT3000 - SAIDA           *
      *    I#GFCT0M - AREA DE COMUNICACAO - SBAT3000 - ERROS           *
      *    I#GFCT3O - AREA DE COMUNICACAO - RESTART                    *
      *    I#GFCTG0 - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCTHA - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCTHW - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCTM0 - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCTDK - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCTDL - AREA DE COMUNICACAO - SAIDA                      *
      *    GFCTWAAR - AREA DE COMUNICACAO - ENTRADA                    *
      *    GFCTWAAS - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCTFZ - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCTG1 - AREA DE COMUNICACAO - SAIDA                      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5054 - GERAR DADOS PARA TEXT DE NOME DE CLIENTES.       *
      *    GFCT5584 - COMBO TARIFA PACOTE - ACOMPANHAMENTO DIARIO.     *
      *    GFCT0194 - LISTA DE DEMONSTRATIVO DE UTILIZACAO DE PACOTE   *
      *    GFCT0198 - DEMONSTRATIVO DO MES FECHADO                     *
      *    GFCT5522 - VERIFICA ONLINE.                                 *
      *    SBAT3000 - COMUNICACAO ENTRE PLATAFORMAS                    *
      *----------------------------------------------------------------*
      *               U L T I M A   A L T E R A C A O                  *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMADOR.........:  HELIO SANTONI     -  SONDA PROCWORK  *
      *    DATA................:  12/09/2008                           *
      *    OBJETIVO............:  INCLUIR PROGRAMA GFCT0198 (DEMONS-   *
      *                           TRATIVO DO MES FECHADO).             *
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

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* INDEXADOR *'.
      *---------------------------------------------------------------*

       77  IND-1                       PIC  9(003) COMP-3  VALUE ZEROS.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREAS AUXILIARES *'.
      *---------------------------------------------------------------*

       77  WRK-GU                      PIC  X(004)         VALUE 'GU'.
       77  WRK-MSGF                    PIC  X(004)         VALUE 'MSGF'.
       77  WRK-MSGI                    PIC  X(004)         VALUE 'MSGI'.
       77  WRK-RSTA                    PIC  X(004)         VALUE 'RSTA'.
       77  WRK-ERRO                    PIC  X(004)         VALUE 'ERRO'.
       77  WRK-FUNCAO                  PIC  X(004)         VALUE SPACES.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-NRO-MSGI                PIC  9(002) COMP-3  VALUE ZEROS.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA DE MENSAGENS *'.
      *---------------------------------------------------------------*

       77  WRK-MSG001                  PIC  X(075)         VALUE
           'ERRO NO SBAT3000 PARA OBTER A MENSAGEM DE ENTRADA'.
       77  WRK-MSG002                  PIC  X(075)         VALUE
           'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - MSGI'.
       77  WRK-MSG003                  PIC  X(075)         VALUE
           'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - MSGF'.
       77  WRK-MSG004                  PIC  X(075)         VALUE
           'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - RSTA'.

       01  WRK-MSG005.
           05  FILLER                  PIC  X(026)         VALUE
               'ERRO NA CHAMADA AO MODULO '.
           05  WRK-MODULO-MSG          PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(041)         VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(070)         VALUE
           '* AREA DE COMUNICACAO COM O ISD (SBAT3000) *'.
      *---------------------------------------------------------------*

       COPY I#ISDDPC.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(070)         VALUE
           '* AREA DE COMUNICACAO - SBAT3000 - ENTRADA *'.
      *---------------------------------------------------------------*

       COPY I#GFCT0K.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(070)         VALUE
           '* AREA DE COMUNICACAO - SBAT3000 - SAIDA *'.
      *---------------------------------------------------------------*

       COPY I#GFCT0L.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(070)         VALUE
           '* AREA DE COMUNICACAO - SBAT3000 - ERROS *'.
      *---------------------------------------------------------------*

       COPY I#GFCT0M.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(070)         VALUE
           '* AREA DE COMUNICACAO - SBAT3000 - RESTART *'.
      *---------------------------------------------------------------*

       COPY I#GFCT3O.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(070)         VALUE
           '* AREA DE COMUNICACAO - ENTRADA  *'.
      *---------------------------------------------------------------*

       COPY I#GFCTG0.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(070)         VALUE
           '* AREA DE COMUNICACAO - SAIDA *'.
      *---------------------------------------------------------------*

       COPY I#GFCTHA.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(070)         VALUE
           '* AREA DE COMUNICACAO - ENTRADA *'.
      *---------------------------------------------------------------*

       COPY I#GFCTHW.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(070)         VALUE
           '* AREA DE COMUNICACAO - SAIDA *'.
      *---------------------------------------------------------------*

       COPY I#GFCTM0.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(070)         VALUE
           '* AREA DE COMUNICACAO - ENTRADA *'.
      *---------------------------------------------------------------*

       COPY I#GFCTDK.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(070)         VALUE
           '* AREA DE COMUNICACAO - SAIDA *'.
      *---------------------------------------------------------------*

       COPY I#GFCTDL.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(070)         VALUE
           '* AREA DE COMUNICACAO - ENTRADA *'.
      *---------------------------------------------------------------*

       COPY GFCTWAAR.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(070)         VALUE
           '* AREA DE COMUNICACAO - SAIDA *'.
      *---------------------------------------------------------------*

       COPY GFCTWAAS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(070)         VALUE
           '* AREA DE COMUNICACAO - ENTRADA *'.
      *---------------------------------------------------------------*

       COPY I#GFCTFZ.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(070)         VALUE
           '* AREA DE COMUNICACAO - SAIDA *'.
      *---------------------------------------------------------------*

       COPY I#GFCTG1.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-IO-PCB.
           05  LNK-IO-TERM             PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  LNK-IO-STA              PIC  X(002).
           05  FILLER                  PIC  X(012).
           05  LNK-IO-MODNAME          PIC  X(008).

       01  LNK-ALT-PCB.
           05  LNK-ALT-PCBNAME         PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  LNK-ALT-STATUS          PIC  X(002).
           05  LNK-ALT-DATE            PIC S9(007) COMP-3.
           05  LNK-ALT-TIME            PIC S9(007) COMP-3.
           05  LNK-ALT-SEQNO           PIC S9(003) COMP.
           05  FILLER                  PIC  X(002).
           05  LNK-ALT-MODNAME         PIC  X(008).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY 'DLITCBL'             USING LNK-IO-PCB
                                             LNK-ALT-PCB.

      ******************************************************************
      *    RECEBE DADOS E CHAMA AS ROTINAS A SEREM EXECUTADAS          *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-GU-MSG-CONTROLE            SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-RECEBER-MENSAGEM.

           PERFORM 2000-VERIFICAR-DISPONIVEL.

           EVALUATE TRUE

               WHEN GFCT0K-FUNCAO      EQUAL 'GFCT5054'

                    PERFORM 3000-PROCESSAR-TEXT-CONTA

               WHEN GFCT0K-FUNCAO      EQUAL 'GFCT5584'
                    MOVE GFCT0K-ENTRADA(1:100)
                                       TO GFCTHW-ENTRADA
                    MOVE SPACES        TO GFCTM0-FIM

                    PERFORM 4000-PROCESSAR-COMBO-TARIFA
                     VARYING IND-1     FROM 1 BY 1
                       UNTIL IND-1     GREATER 3
                          OR GFCTM0-FIM
                                       EQUAL 'S'

               WHEN GFCT0K-FUNCAO      EQUAL 'GFCT0194'
                    MOVE GFCT0K-ENTRADA(1:150)
                                       TO GFCTDK-ENTRADA
                    MOVE SPACES        TO GFCTDL-FIM

                    PERFORM 5000-PROCESSAR-LISTA-DEMONST
                     VARYING IND-1     FROM 1 BY 1
                       UNTIL IND-1     GREATER 3
                          OR GFCTDL-FIM
                                       EQUAL 'S'

               WHEN GFCT0K-FUNCAO      EQUAL 'GFCT0198'
                    MOVE GFCT0K-ENTRADA(1:150)
                                       TO GFCTWAAR-ENTRADA
                    MOVE SPACES        TO GFCTWAAS-FIM

                    PERFORM 6000-PROCESSAR-EXTR-MES-FECH
                     VARYING IND-1     FROM 1 BY 1
                       UNTIL IND-1     GREATER 3
                          OR GFCTWAAS-FIM
                                       EQUAL 'S'

           END-EVALUATE.

           GO TO 0000-GU-MSG-CONTROLE.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    RECEBE MENSAGEM VIA SBAT3000                                *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-RECEBER-MENSAGEM           SECTION.
      *----------------------------------------------------------------*

           CALL 'SBAT3000'             USING WRK-GU
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             MCA-ISDPA
                                             GFCT0K-ENTRADA.

           IF  MPA-RCODE-API           EQUAL 'QC'
               PERFORM 9000-FINALIZAR
           END-IF.

           IF  MPA-RCODE-API           NOT EQUAL SPACES
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT7193'         TO GFCT0M-PROGRAMA
               MOVE '02'               TO GFCT0L-MPARCODE-APLIC
               PERFORM 9200-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICAR DISPONIVEL                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-DISPONIVEL       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  GFCTG1-SAIDA.

           MOVE 'GFCT5522'             TO WRK-MODULO.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE GFCT0K-TRANSACAO       TO GFCTFZ-TRANSACAO.
           MOVE GFCT0K-FUNCAO          TO GFCTFZ-FUNCAO.
           MOVE ZEROS                  TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.

           CALL WRK-MODULO             USING GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               PERFORM 9300-TRATAR-ERRO-MODULO
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 9
               PERFORM 9200-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 1
               MOVE 'SISTEMA INDISPONIVEL'
                                       TO GFCT0M-TEXTO
               PERFORM 9300-TRATAR-ERRO-MODULO
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAR TEXT CONTA                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR-TEXT-CONTA       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                     GFCTG0-ENTRADA
           MOVE GFCT0K-ENTRADA(1:100)  TO GFCTG0-ENTRADA

           IF  GFCTG0-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTHA-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE                 GFCT0L-RET-WEB
                                          GFCTHA-SAIDA
                                          GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 3100-CHAMAR-GFCT5054.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CHAMAR PROGRAMA GFCT5054                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-CHAMAR-GFCT5054            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTG0-FUNCAO          TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTG0-ENTRADA
                                             GFCTHA-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               PERFORM 9300-TRATAR-ERRO-MODULO
           END-IF.

           IF  GFCTHA-ERRO             EQUAL 9
               MOVE GFCTHA-DESC-MSG-ERRO(1:75)
                                       TO GFCT0M-TEXTO
               PERFORM 9100-FORMATAR-ERRO-APL-DB2
           END-IF.

           PERFORM 3110-TRATAR-RET-5054.

           MOVE GFCTHA-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 3120-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TRATAR RETORNO 5054                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       3110-TRATAR-RET-5054            SECTION.
      *----------------------------------------------------------------*

           IF  GFCTHA-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               MOVE WRK-MSGI           TO GFCT0L-TPOMSG-WEB
           END-IF.

           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       3110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CHAMAR SBAT300                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       3120-CHAMAR-SBAT3000            SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FUNCAO              EQUAL WRK-RSTA
               CALL 'SBAT3000'         USING WRK-FUNCAO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             MCA-ISDPA
                                             GFCT0L-ENTRADA
                                             GFCT3O-AREA-RESTART
           ELSE
               CALL 'SBAT3000'         USING WRK-FUNCAO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             MCA-ISDPA
                                             GFCT0L-ENTRADA
           END-IF.

           IF  MPA-RCODE-API           NOT EQUAL SPACES
               IF  WRK-FUNCAO          EQUAL 'MSGI'
                   MOVE WRK-MSG002     TO GFCT0M-TEXTO
               ELSE
                   IF  WRK-FUNCAO      EQUAL 'MSGF'
                       MOVE WRK-MSG003 TO GFCT0M-TEXTO
                   ELSE
                       MOVE WRK-MSG004 TO GFCT0M-TEXTO
                   END-IF
               END-IF
               MOVE 'GFCT7193'         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE '02'               TO GFCT0L-MPARCODE-APLIC
               PERFORM 9200-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAR COMBO TARIFA
      ******************************************************************
      *----------------------------------------------------------------*
       4000-PROCESSAR-COMBO-TARIFA     SECTION.
      *----------------------------------------------------------------*

           IF  GFCTHW-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTM0-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTM0-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 4100-CHAMAR-GFCT5584.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CHAMAR PROGRAMA GFCT5584                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-CHAMAR-GFCT5584            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTHW-FUNCAO          TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTHW-ENTRADA
                                             GFCTM0-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               PERFORM 9300-TRATAR-ERRO-MODULO
           END-IF.

           IF  GFCTM0-ERRO             EQUAL 9
               PERFORM 9100-FORMATAR-ERRO-APL-DB2
           END-IF.

           PERFORM 4110-TRATAR-RET-5584.

           PERFORM 4120-RETORNAR-WEB-5584.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TRATAR RETORNO COMBO TARIFA
      ******************************************************************
      *----------------------------------------------------------------*
       4110-TRATAR-RET-5584            SECTION.
      *----------------------------------------------------------------*

           IF  GFCTM0-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               MOVE GFCTM0-PONTEIRO    TO GFCTHW-PONTEIRO
               MOVE GFCTM0-QTDE-OCOR   TO GFCTHW-QTDE-OCOR
               MOVE ZEROS              TO GFCTHW-QTDE-TOT-REG
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR
               GFCT0L-TPOMSG-WEB       EQUAL WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               ADD 1                   TO WRK-NRO-MSGI
               MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       4110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    RETORNAR WEB 5584                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       4120-RETORNAR-WEB-5584          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTM0-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL WRK-RSTA
               MOVE 255                TO GFCT3O-LL
               MOVE GFCTM0-FUNCAO      TO GFCT3O-FUNCAO
               MOVE GFCTM0-QTDE-OCOR   TO GFCTHW-QTDE-OCOR
               MOVE ZEROS              TO GFCTHW-QTDE-TOT-REG
               MOVE GFCTM0-FUNC-BDSCO  TO GFCTHW-FUNC-BDSCO
               MOVE GFCTM0-PONTEIRO    TO GFCTHW-PONTEIRO
               MOVE GFCTM0-FIM         TO GFCTHW-FIM
               MOVE GFCTHW-DADOS-RST(1:79)
                                       TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 3120-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       4120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAR LISTA DEMONSTRATIVO DE UTILIZACAO DE PACOTE       *
      ******************************************************************
      *----------------------------------------------------------------*
       5000-PROCESSAR-LISTA-DEMONST    SECTION.
      *----------------------------------------------------------------*

           IF  GFCTDK-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTDL-SAIDA
                                          GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTDL-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 5100-CHAMAR-GFCT0194.

      *----------------------------------------------------------------*
       5000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      ******************************************************************
      *    CHAMAR PROGRAMA GFCT0194                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       5100-CHAMAR-GFCT0194            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTDK-FUNCAO          TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTDK-ENTRADA
                                             GFCTDL-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               PERFORM 9300-TRATAR-ERRO-MODULO
           END-IF.

           IF  GFCTDL-ERRO             EQUAL 9
               PERFORM 9100-FORMATAR-ERRO-APL-DB2
           END-IF.

           PERFORM 5110-TRATAR-RET-GFCT0194.

           PERFORM 5120-RETORNAR-WEB-GFCT0194.

      *----------------------------------------------------------------*
       5100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      ******************************************************************
      *    TRATAR RETORNO GFCT0914                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       5110-TRATAR-RET-GFCT0194        SECTION.
      *----------------------------------------------------------------*

           IF  GFCTDL-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               MOVE GFCTDL-FILTRO      TO GFCTDK-FILTRO
               MOVE GFCTDL-PONTEIRO    TO GFCTDK-PONTEIRO

               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR
               GFCT0L-TPOMSG-WEB       EQUAL WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               ADD 1                   TO WRK-NRO-MSGI
               MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       5110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      ******************************************************************
      *    RETORNAR WEB GFCT0194                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       5120-RETORNAR-WEB-GFCT0194      SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTDL-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL WRK-RSTA
               MOVE 255                TO GFCT3O-LL
               MOVE GFCTDL-FUNCAO      TO GFCT3O-FUNCAO
               MOVE GFCTDL-QTDE-OCOR   TO GFCTDK-QTDE-OCOR
               MOVE ZEROS              TO GFCTDK-QTDE-TOT-REG
               MOVE GFCTDL-FUNC-BDSCO  TO GFCTDK-FUNC-BDSCO
               MOVE GFCTDL-FILTRO      TO GFCTDK-FILTRO
               MOVE GFCTDL-PONTEIRO    TO GFCTDK-PONTEIRO
               MOVE GFCTDL-FIM         TO GFCTDK-FIM
               MOVE GFCTDK-DADOS-RST   TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 3120-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       5120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAR LISTA DEMONSTRATIVO DO MES FECHADO                *
      ******************************************************************
      *----------------------------------------------------------------*
       6000-PROCESSAR-EXTR-MES-FECH    SECTION.
      *----------------------------------------------------------------*

           IF  GFCTWAAR-FIM            EQUAL 'N'
               MOVE SPACES             TO GFCTWAAS-SAIDA
                                          GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTWAAS-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 6100-CHAMAR-GFCT0198.

      *----------------------------------------------------------------*
       6000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      ******************************************************************
      *    CHAMAR PROGRAMA GFCT0198                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       6100-CHAMAR-GFCT0198            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTWAAR-FUNCAO        TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTWAAR-ENTRADA
                                             GFCTWAAS-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               PERFORM 9300-TRATAR-ERRO-MODULO
           END-IF.

           IF  GFCTWAAS-ERRO           EQUAL 9
               PERFORM 9100-FORMATAR-ERRO-APL-DB2
           END-IF.

           PERFORM 6110-TRATAR-RET-GFCT0198

           PERFORM 6120-RETORNAR-WEB-GFCT0198.

      *----------------------------------------------------------------*
       6100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TRATAR RETORNO GFCT0918                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       6110-TRATAR-RET-GFCT0198        SECTION.
      *----------------------------------------------------------------*

           IF  GFCTWAAS-FIM            EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               MOVE GFCTWAAS-FILTRO    TO GFCTWAAR-FILTRO
               MOVE GFCTWAAS-PONTEIRO  TO GFCTWAAR-PONTEIRO
               MOVE GFCTWAAS-ACUMULADOR
                                       TO GFCTWAAR-ACUMULADOR

               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR
               GFCT0L-TPOMSG-WEB       EQUAL WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               ADD 1                   TO WRK-NRO-MSGI
               MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       6110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    RETORNAR WEB GFCT0198                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       6120-RETORNAR-WEB-GFCT0198      SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTWAAS-SAIDA         TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL WRK-RSTA
               MOVE 255                TO GFCT3O-LL
               MOVE GFCTWAAS-FUNCAO    TO GFCT3O-FUNCAO
               MOVE GFCTWAAS-QTDE-OCOR TO GFCTWAAR-QTDE-OCOR
               MOVE ZEROS              TO GFCTWAAR-QTDE-TOT-REG
               MOVE GFCTWAAS-FUNC-BDSCO
                                       TO GFCTWAAR-FUNC-BDSCO
               MOVE GFCTWAAS-FILTRO    TO GFCTWAAR-FILTRO
               MOVE GFCTWAAS-PONTEIRO  TO GFCTWAAR-PONTEIRO
               MOVE GFCTWAAS-ACUMULADOR
                                       TO GFCTWAAR-ACUMULADOR
               MOVE GFCTWAAS-FIM       TO GFCTWAAR-FIM
               MOVE GFCTWAAR-DADOS-RST TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 3120-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       6120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FINALIZA PROCESSAMENTO                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       9000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9100-FORMATAR-ERRO-APL-DB2      SECTION.
      *----------------------------------------------------------------*

           IF  GFCT0M-TIPO-ACESSO      NOT EQUAL 'DB2'
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
           END-IF.

           MOVE '02'                   TO MPA-RCODE-APLICACAO.

           PERFORM 9200-PROCESSAR-ROTINA-ERRO.

      *----------------------------------------------------------------*
       9100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSA ERROS DO PROGRAMA                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       9200-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT7193'             TO GFCT0M-TRANSACAO.
           MOVE GFCT0L-MPARCODE-APLIC  TO MPA-RCODE-APLICACAO.

           IF  GFCT0M-SQLCA-AREA       EQUAL SPACES OR LOW-VALUES
               CALL 'SBAT3000'         USING WRK-ERRO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             MCA-ISDPA
                                             GFCT0M-ERRO-AREAS
           ELSE
               CALL 'SBAT3000'         USING WRK-ERRO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             MCA-ISDPA
                                             GFCT0M-ERRO-AREAS
                                             GFCT0M-SQLCA-AREA
           END-IF.

           PERFORM 9000-FINALIZAR.

      *----------------------------------------------------------------*
       9200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TRATA RETORNO DE ERRO NOS ACESSOS AOS MODULOS               *
      ******************************************************************
      *----------------------------------------------------------------*
       9300-TRATAR-ERRO-MODULO         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MODULO             TO WRK-MODULO-MSG
                                          GFCT0M-PROGRAMA.
           MOVE WRK-MSG005             TO GFCT0M-TEXTO.
           MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO.
           MOVE '02'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 9200-PROCESSAR-ROTINA-ERRO.

      *----------------------------------------------------------------*
       9300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
