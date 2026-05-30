      *----------------------------------------------------------------*
       IDENTIFICATION                  DIVISION.
      *----------------------------------------------------------------*

       PROGRAM-ID.          GFCT7270.
       AUTHOR.              JEFERSON.

      *================================================================*
      *         S T E F A N I N I       C O N S U L T O R I A          *
      *================================================================*
      *                                                                *
      *      PROGRAMA     : GFCT7270                                   *
      *      PROGRAMADOR..: JEFERSON AMARAL            -  STEFANINI    *
      *      ACQ..........: RICARDO PINHO              -  STEFANINI    *
      *      ANALISTA.....: RENATO TAMANAHA            -  PROCWORK     *
      *      DATA.........: 30/03/2005                                 *
      *                                                                *
      *----------------------------------------------------------------*
      *                                                                *
      *      OBJETIVO.....: EFETUAR COMUNICACAO ENTRE WEB E MF VIA     *
      *                     SBAT3000 PARA AS CHAMADAS NOS  MODULOS     *
      *                     REFERENTES A FUNCIONALIDADE REMESSA PA-    *
      *                     RA APROVACAO DA TARIFA.                    *
      *                                                                *
      *----------------------------------------------------------------*
      *                                                                *
      *      MODULOS:                                                  *
      *                                                                *
      *      GFCT0271  -  INCLUSAO REMESSA PARA APROVACAO.             *
      *      GFCT5000  -  OBTER DESCRICAO DE MENSAGENS.                *
      *      GFCT5502  -  VERIFICACAO GESTOR MASTER.                   *
      *      GFCT5505  -  VERIFICACAO GESTOR LOGADO.                   *
      *      GFCT5500  -  VERIFICACAO DE DISPONIBILIDADE ON-LINE.      *
      *      GFCT5031  -  COMBO DE TARIFAS.                            *
      *      POOL0081  -  ALOCAR DINAMICAMENTE UM MODULO.              *
      *      SBAT3000  -  COMUNICACAO ENTRE PLATAFORMAS.               *
      *                                                                *
      *----------------------------------------------------------------*
      *                                                                *
      *      INCS   :                                                  *
      *                                                                *
      *      I#GFCT6P  -  AREA DE COMUNICACAO - ENTRADA/SAIDA CONSULTA *
      *      I#GFCT7L  -  COMBO DE TARIFAS - ENTRADA                   *
      *      I#GFCT7M  -  COMBO DE TARIFAS - SAIDA                     *
      *      I#GFCT5O  -  AREA DE COMUNICACAO - SAIDA RESTART          *
      *      I#GFCT4R  -  AREA DE COMUNICACAO - ENTRADA CONSULTA GESTOR*
      *                                         MASTER                 *
      *      I#GFCT4S  -  AREA DE COMUNICACAO - SAIDA CONSULTA GESTOR  *
      *                                         MASTER                 *
      *      I#GFCT0K  -  AREA DE COMUNICACAO - SBAT3000 - ENTRADA     *
      *      I#GFCT0L  -  AREA DE COMUNICACAO - SBAT3000 - SAIDA       *
      *      I#GFCT0M  -  AREA DE COMUNICACAO - SBAT3000 - ERROS       *
      *      I#GFCT3O  -  AREA DE COMUNICACAO - SBAT3000 - RESTART     *
      *      I#ISDDPC  -  AREA DE COMUNICACAO - SBAT3000 - ISD         *
      *                                                                *
      *================================================================*

      *----------------------------------------------------------------*
       ENVIRONMENT                     DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
       SPECIAL-NAMES.

           DECIMAL-POINT       IS      COMMA.

      *----------------------------------------------------------------*
       DATA                            DIVISION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

       77  FILLER                      PIC  X(32) VALUE
           'INICIO DA WORKING GFCT7270'.

      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    AREA DE COMUNICACAO COM O SBAT3000                          *
      *----------------------------------------------------------------*

-INC I#ISDDPC

      *----------------------------------------------------------------*
      *    AREA DE COMUNICACAO COM OS MODULOS                          *
      *----------------------------------------------------------------*

-INC 'I#GFCT6P'

-INC 'I#GFCT7L'

-INC 'I#GFCT7M'

-INC 'I#GFCT5O'

-INC 'I#GFCT4R'

-INC 'I#GFCT4S'
-INC 'I#GFCTEC'
-INC 'I#GFCTED'

      *----------------------------------------------------------------*
      *    AREA DE COMUNICACAO COM A WEB   -   ENTRADA  TRANSACAO      *
      *----------------------------------------------------------------*

-INC I#GFCT0K

      *----------------------------------------------------------------*
      *    AREA DE COMUNICACAO COM A WEB   -   SAIDA    TRANSACAO      *
      *----------------------------------------------------------------*

-INC I#GFCT0L

      *----------------------------------------------------------------*
      *    AREA DE COMUNICACAO COM A WEB   -   ERROS                   *
      *----------------------------------------------------------------*

-INC I#GFCT0M

      *----------------------------------------------------------------*
      *    AREA DE COMUNICACAO COM A WEB   -   RESTART                 *
      *----------------------------------------------------------------*

-INC I#GFCT3O

      *----------------------------------------------------------------*
      *    AREAS AUXILIARES                                            *
      *----------------------------------------------------------------*

       01  FILLER.
           03  WRK-IND                 PIC  9(03) VALUE ZEROS.

      *----------------------------------------------------------------*
      *    AREA DE CONTROLE                                            *
      *----------------------------------------------------------------*

       01  FILLER.
           03  WRK-MODULO              PIC  X(08) VALUE SPACES.
           03  WRK-GU                  PIC  X(04) VALUE 'GU'.
           03  WRK-ERRO                PIC  X(04) VALUE 'ERRO'.
           03  WRK-FUNCAO              PIC  X(04) VALUE SPACES.
           03  WRK-FINALIZAR           PIC  X(01) VALUE 'N'.
           03  WRK-NRO-MSGI-H          PIC  9(03) VALUE ZEROS.
           03  WRK-NRO-MSGI-J          PIC  9(03) VALUE ZEROS.

      *----------------------------------------------------------------*
      *    AREA DE MENSAGENS                                           *
      *----------------------------------------------------------------*

       01  WRK-MENSAGENS.
           03  WRK-MSG01               PIC  X(75) VALUE
               'ERRO NO SBAT3000 PARA OBTER A MENSAGEM DE ENTRADA'.
           03  WRK-MSG02               PIC  X(75) VALUE
               'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - MSGI'.
           03  WRK-MSG03               PIC  X(75) VALUE
               'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - MSGF'.
           03  WRK-MSG04               PIC  X(75) VALUE
               'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - RSTA'.
           03  WRK-MSG05.
               05  FILLER              PIC  X(026)         VALUE
               'ERRO NA CHAMADA AO MODULO '.
               05  WRK-MODULO-MSG      PIC  X(008)         VALUE SPACES.

MBA    01  WRK-MSG-ERRO.
MBA        05 FILLER                   PIC  X(037) VALUE SPACES.
MBA        05 WRK-TABELA-ERRO          PIC  X(008) VALUE SPACES.

       01  WRK-5500-AREA-SAIDA.
           03  WRK-5500-COD-RETORNO    PIC  9(02).
           03  WRK-5500-COD-SQL-ERRO   PIC  9(03).
           03  WRK-5500-DESC-MSG       PIC  X(70).

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32) VALUE
           '* AREA DO MODULO GFCT5505 *'.
      *----------------------------------------------------------------*

       01  WRK-5505-AREA-ENTRADA.
           03 WRK-5505-AMBIENTE        PIC X(01)         VALUE SPACES.
           03 WRK-5505-FUNC-BDSCO      PIC X(07)         VALUE SPACES.

       01  WRK-5505-AREA-SAIDA.
           03 WRK-5505-COD-RETORNO     PIC 9(02)         VALUE ZEROS.
           03 WRK-5505-COD-SQL-ERRO    PIC 9(03)         VALUE ZEROS.
           03 WRK-5505-DESC-MSG        PIC X(70)         VALUE ZEROS.
           03 WRK-5505-DADOS-RETORNO.
              05 WRK-5505-COD-JUNCAO   PIC 9(05)         VALUE ZEROS.

       01  FILLER                      PIC  X(32) VALUE
           'FIM    DA WORKING GFCT7270'.

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  IO-PCB.
           03  IO-LTERM                PIC  X(08).
           03  FILLER                  PIC  X(02).
           03  IO-STATUS               PIC  X(02).
           03  FILLER                  PIC  X(12).
           03  IO-MODNAME              PIC  X(08).

       01  ALT-PCB.
           03  ALT-PCBNAME             PIC  X(08).
           03  FILLER                  PIC  X(02).
           03  ALT-STATUS              PIC  X(02).
           03  ALT-DATE                PIC S9(07) COMP-3.
           03  ALT-TIME                PIC S9(07) COMP-3.
           03  ALT-SEQNO               PIC S9(03) COMP.
           03  FILLER                  PIC  X(02).
           03  ALT-MODNAME             PIC  X(08).

      *----------------------------------------------------------------*
       PROCEDURE                       DIVISION.
      *----------------------------------------------------------------*

           ENTRY   'DLITCBL'           USING   IO-PCB
                                               ALT-PCB.

      *----------------------------------------------------------------*
       10000-PROCESSAMENTO             SECTION.
      *----------------------------------------------------------------*

           CALL  'SBAT3000'            USING   WRK-GU
                                               IO-PCB
                                               ALT-PCB
                                               MCA-ISDPA
                                               GFCT0K-ENTRADA.

           IF  MPA-RCODE-API           EQUAL   'QC'
               PERFORM  33000-FINAL
           END-IF.

           IF  MPA-RCODE-API       NOT EQUAL   SPACES
               MOVE  WRK-MSG01     TO      GFCT0M-TEXTO
               MOVE  'APL'         TO      GFCT0M-TIPO-ACESSO
               MOVE  'SBAT3000'    TO      GFCT0M-TRANSACAO
               MOVE  '02'          TO      GFCT0L-MPARCODE-APLIC
               PERFORM  32000-TRATAR-ERRO
           END-IF.

           PERFORM  20000-VERIFICA-SISTEMA-DISP.

           PERFORM  30000-PROCESSAR.

           GO       TO     10000-PROCESSAMENTO.

      *----------------------------------------------------------------*
       10000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       20000-VERIFICA-SISTEMA-DISP     SECTION.
      *----------------------------------------------------------------*

      ** SITUACAO DO SISTEMA - ( LIBERADO OU BLOQUEADO )

           MOVE  'GFCT5500'            TO      WRK-MODULO.

           CALL  'POOL0081'            USING   WRK-MODULO
                                               WRK-5500-AREA-SAIDA.

MBA        IF  RETURN-CODE                    NOT EQUAL ZEROS
MBA            MOVE WRK-MODULO                TO WRK-MODULO-MSG
MBA                                              GFCT0M-PROGRAMA
MBA            MOVE WRK-MSG05                 TO GFCT0M-TEXTO
MBA            MOVE 'APL'                     TO GFCT0M-TIPO-ACESSO
MBA            MOVE '02'                      TO GFCT0L-MPARCODE-APLIC
MBA            PERFORM  32000-TRATAR-ERRO
MBA        END-IF
MBA
MBA        IF  WRK-5500-COD-RETORNO           NOT EQUAL ZEROS
MBA            IF  WRK-5500-COD-RETORNO       EQUAL 4
MBA                MOVE 'APL'                 TO GFCT0M-TIPO-ACESSO
MBA                MOVE 'GFCT5500'            TO GFCT0M-PROGRAMA
MBA                MOVE 'SIST. INDISPONIVEL'  TO GFCT0M-TEXTO
MBA            ELSE
MBA                MOVE 'DB2'                 TO GFCT0M-TIPO-ACESSO
MBA                MOVE 'GFCT5500'            TO GFCT0M-PROGRAMA
MBA                MOVE WRK-5500-COD-SQL-ERRO TO GFCT0M-SQLCODE
MBA                MOVE 'SELECT'              TO GFCT0M-COMANDO-SQL
MBA                MOVE '0001'                TO GFCT0M-LOCAL
MBA                MOVE WRK-5500-DESC-MSG     TO WRK-MSG-ERRO
MBA                IF  WRK-TABELA-ERRO        EQUAL 'GFCTB0A1'
MBA                    MOVE 'PARM_DATA_PROCM' TO GFCT0M-NOME-TAB
MBA                ELSE
MBA                    MOVE 'PARM_CTRL_GSTAO' TO GFCT0M-NOME-TAB
MBA                END-IF
MBA            END-IF
MBA            MOVE '02'                      TO GFCT0L-MPARCODE-APLIC
MBA            PERFORM  32000-TRATAR-ERRO
MBA        END-IF.

           IF  GFCT0K-FUNCAO           EQUAL   'GFCT5502'
               PERFORM 031100-OBTER-DEP-USUARIO
               MOVE  GFCT4R-FUNCAO     TO WRK-MODULO
               MOVE SPACES             TO GFCT4R-ENTRADA
                                          GFCT4S-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE GFCT4R-ENTRADA
                          GFCT4S-SAIDA
                          GFCT0M-AREA-ERROS
               MOVE GFCT0K-ENTRADA         TO GFCT4R-ENTRADA
               MOVE WRK-5505-COD-JUNCAO    TO GFCT4R-CJUNC-DEPDC
               CALL  'POOL0081'        USING   WRK-MODULO
                                               GFCT4R-ENTRADA
                                               GFCT4S-SAIDA
                                               GFCT0M-AREA-ERROS
               IF  RETURN-CODE     NOT EQUAL   ZEROS
                   MOVE  WRK-MSG05     TO      GFCT0M-TEXTO
                   MOVE  'APL'         TO      GFCT0M-TIPO-ACESSO
                   MOVE  'GFCT5502'    TO      GFCT0M-PROGRAMA
                   MOVE  '02'          TO      GFCT0L-MPARCODE-APLIC
                   PERFORM  32000-TRATAR-ERRO
               END-IF
               IF  GFCT4S-ERRO         EQUAL   9
                   IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                       MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                       MOVE  GFCT4S-FUNCAO TO GFCT0M-PROGRAMA
                       MOVE  GFCT4S-DESC-MSG-ERRO
                                           TO GFCT0M-TEXTO
                   END-IF
                   MOVE '02'               TO GFCT0L-MPARCODE-APLIC
                   PERFORM  32000-TRATAR-ERRO
               END-IF
               PERFORM  20100-FORMATA-AREA
               PERFORM  20200-RETORNA-WEB
           END-IF.

      *----------------------------------------------------------------*
       20000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       20100-FORMATA-AREA              SECTION.
      *----------------------------------------------------------------*

           MOVE  GFCT4S-SAIDA          TO  GFCT0L-ENTRADA.
           MOVE  +1010                 TO  GFCT0L-LL.
           MOVE  '00'                  TO  MPA-RCODE-APLICACAO.
           MOVE  'MSGF'                TO  WRK-FUNCAO.

      *----------------------------------------------------------------*
       20100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       20200-RETORNA-WEB               SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FUNCAO              EQUAL   'RSTA'
               CALL  'SBAT3000'        USING   WRK-FUNCAO
                                               IO-PCB
                                               ALT-PCB
                                               MCA-ISDPA
                                               GFCT0L-ENTRADA
                                               GFCT3O-AREA-RESTART
           ELSE
               CALL  'SBAT3000'        USING   WRK-FUNCAO
                                               IO-PCB
                                               ALT-PCB
                                               MCA-ISDPA
                                               GFCT0L-ENTRADA
           END-IF.
           IF  MPA-RCODE-API       NOT EQUAL   SPACES
               IF  WRK-FUNCAO          EQUAL   'MSGI'
                   MOVE  WRK-MSG02     TO      GFCT0M-TEXTO
               ELSE
                   IF  WRK-FUNCAO      EQUAL   'MSGF'
                       MOVE  WRK-MSG03 TO      GFCT0M-TEXTO
                   ELSE
                       MOVE  WRK-MSG04 TO      GFCT0M-TEXTO
                   END-IF
               END-IF
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'SBAT3000'         TO GFCT0M-PROGRAMA
               MOVE '02'               TO GFCT0L-MPARCODE-APLIC
               PERFORM  32000-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       20200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       30000-PROCESSAR                 SECTION.
      *----------------------------------------------------------------*

           MOVE  GFCT0K-ENTRADA        TO      GFCT6P-SAIDA.
           MOVE  GFCT0K-ENTRADA        TO      GFCTEC-ENTRADA.

           IF  GFCT0K-FUNCAO           EQUAL   'GFCT0271'
               PERFORM  30100-PROCESSAR-INCLUSAO
               PERFORM  33000-FINAL
           END-IF.

           IF  GFCT0K-FUNCAO           EQUAL   'GFCT5080'
               MOVE  08                TO GFCTEC-CSIT-LIBRC-TARIF
               MOVE  1                 TO WRK-NRO-MSGI-H
               PERFORM  40000-COMBO-DE-TARIFAS
                        VARYING WRK-IND FROM 1 BY 1
                        UNTIL GFCTED-FIM EQUAL  'S'
                        OR WRK-IND GREATER 3
               PERFORM  33000-FINAL
           END-IF.

      *----------------------------------------------------------------*
       30000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       30100-PROCESSAR-INCLUSAO        SECTION.
      *----------------------------------------------------------------*

           MOVE  GFCT6P-FUNCAO         TO      WRK-MODULO.

           CALL  'POOL0081'            USING   WRK-MODULO
                                               GFCT6P-SAIDA
                                               GFCT0M-AREA-ERROS.

           IF  RETURN-CODE         NOT EQUAL   ZEROS
               MOVE  WRK-MSG05         TO      GFCT0M-TEXTO
               MOVE  'APL'             TO      GFCT0M-TIPO-ACESSO
               MOVE  WRK-MODULO        TO      GFCT0M-PROGRAMA
               MOVE  '02'              TO      GFCT0L-MPARCODE-APLIC
               PERFORM  32000-TRATAR-ERRO
           END-IF.

           IF  GFCT6P-ERRO             EQUAL   9
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE  GFCT6P-FUNCAO TO GFCT0M-PROGRAMA
                   MOVE  GFCT6P-DESC-MSG-ERRO
                                       TO GFCT0M-TEXTO
               END-IF
               MOVE '02'               TO GFCT0L-MPARCODE-APLIC
               PERFORM  32000-TRATAR-ERRO
           END-IF.

           PERFORM  30200-TRATAR-RETORNO.

           PERFORM  30300-RETORNAR-PARA-WEB.

      *----------------------------------------------------------------*
       30100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       30200-TRATAR-RETORNO            SECTION.
      *----------------------------------------------------------------*

           IF  GFCT6P-FIM              EQUAL   'S'
               MOVE  'MSGF'            TO      GFCT0L-TPOMSG-WEB
           ELSE
               MOVE  'MSGI'            TO      GFCT0L-TPOMSG-WEB
               IF  WRK-NRO-MSGI-H      GREATER 2
                   MOVE  'RSTA'        TO      GFCT0L-TPOMSG-WEB
               ELSE
                   ADD   1             TO      WRK-NRO-MSGI-H
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL   'RSTA'  OR  'MSGF'
               MOVE  ZEROS             TO      WRK-NRO-MSGI-H
           END-IF.

           MOVE  '00'                  TO      GFCT0L-MPARCODE-APLIC.
           MOVE  GFCT0L-TPOMSG-WEB     TO      WRK-FUNCAO.

      *----------------------------------------------------------------*
       30200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       30300-RETORNAR-PARA-WEB         SECTION.
      *----------------------------------------------------------------*

           MOVE  GFCT6P-SAIDA          TO      GFCT0L-ENTRADA.
           MOVE  256                   TO      GFCT0L-LL.
           MOVE  GFCT6P-FUNCAO         TO      GFCT3O-FUNCAO.
           MOVE  GFCT6P-DADOS          TO      GFCT3O-DADOS-RST.

           IF  WRK-FUNCAO              EQUAL   'RSTA'
               CALL  'SBAT3000'        USING   WRK-FUNCAO
                                               IO-PCB
                                               ALT-PCB
                                               MCA-ISDPA
                                               GFCT0L-ENTRADA
                                               GFCT3O-AREA-RESTART
           ELSE
               CALL  'SBAT3000'        USING   WRK-FUNCAO
                                               IO-PCB
                                               ALT-PCB
                                               MCA-ISDPA
                                               GFCT0L-ENTRADA
           END-IF.

           IF  MPA-RCODE-API       NOT EQUAL   SPACES
               IF  WRK-FUNCAO          EQUAL   'MSGI'
                   MOVE  WRK-MSG02     TO      GFCT0M-TEXTO
               ELSE
                   IF  WRK-FUNCAO      EQUAL   'MSGF'
                       MOVE  WRK-MSG03 TO      GFCT0M-TEXTO
                   ELSE
                       MOVE  WRK-MSG04 TO      GFCT0M-TEXTO
                   END-IF
               END-IF
               MOVE  'APL'             TO      GFCT0M-TIPO-ACESSO
               MOVE  'SBAT3000'        TO      GFCT0M-TRANSACAO
               MOVE  '02'              TO      GFCT0L-MPARCODE-APLIC
               PERFORM  32000-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       30300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       031100-OBTER-DEP-USUARIO         SECTION.
      *----------------------------------------------------------------*
           MOVE GFCT0K-ENTRADA    TO GFCT4R-ENTRADA.
           MOVE GFCT4R-FUNC-BDSCO TO WRK-5505-FUNC-BDSCO


           MOVE 'O'                    TO WRK-5505-AMBIENTE.
           MOVE 'GFCT5505'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5505-AREA-ENTRADA
                                             WRK-5505-AREA-SAIDA
                                             IO-PCB
                                             ALT-PCB.

           IF  WRK-5505-COD-RETORNO    NOT EQUAL ZEROS
               MOVE WRK-5505-COD-RETORNO
                                       TO GFCT0M-STACODE
               MOVE WRK-5505-DESC-MSG  TO GFCT0M-TEXTO
               MOVE  'APL'             TO      GFCT0M-TIPO-ACESSO
               MOVE  'GFCT5505'        TO      GFCT0M-PROGRAMA
               MOVE  '02'              TO      GFCT0L-MPARCODE-APLIC
               PERFORM 32000-TRATAR-ERRO
           END-IF.

           MOVE WRK-5505-COD-JUNCAO    TO GFCT4R-CJUNC-DEPDC.

      *----------------------------------------------------------------*
       031100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       32000-TRATAR-ERRO               SECTION.
      *----------------------------------------------------------------*

           MOVE  'GFCT7270'            TO      GFCT0M-TRANSACAO.
           MOVE  GFCT0L-MPARCODE-APLIC TO      MPA-RCODE-APLICACAO.

           IF  GFCT0M-SQLCA-AREA       EQUAL   SPACES  OR  LOW-VALUES
               CALL  'SBAT3000'        USING   WRK-ERRO
                                               IO-PCB
                                               ALT-PCB
                                               MCA-ISDPA
                                               GFCT0M-ERRO-AREAS
           ELSE
               CALL  'SBAT3000'        USING   WRK-ERRO
                                               IO-PCB
                                               ALT-PCB
                                               MCA-ISDPA
                                               GFCT0M-ERRO-AREAS
                                               GFCT0M-SQLCA-AREA
           END-IF.

           PERFORM  33000-FINAL.

      *----------------------------------------------------------------*
       32000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       33000-FINAL                     SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       33000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       40000-COMBO-DE-TARIFAS          SECTION.
      *----------------------------------------------------------------*
********   DISPLAY '6 COMBO DE TARIFAS '
CPM********MOVE  GFCT0K-ENTRADA        TO      GFCTEC-ENTRADA.
           MOVE  GFCTEC-FUNCAO         TO      WRK-MODULO.

           CALL  'POOL0081'            USING   WRK-MODULO
                                               GFCTEC-ENTRADA
                                               GFCTED-SAIDA
                                               GFCT0M-AREA-ERROS.

           IF  RETURN-CODE         NOT EQUAL   ZEROS
               MOVE  WRK-MSG05         TO      GFCT0M-TEXTO
               MOVE  'APL'             TO      GFCT0M-TIPO-ACESSO
               MOVE  'GFCT7270'        TO      GFCT0M-PROGRAMA
               MOVE  '02'              TO      GFCT0L-MPARCODE-APLIC
               PERFORM  32000-TRATAR-ERRO
           END-IF.

           IF  GFCTED-ERRO             EQUAL   9
               MOVE  'APL'             TO      GFCT0M-TIPO-ACESSO
               MOVE  'GFCT7270'        TO      GFCT0M-PROGRAMA
               MOVE  '02'              TO      GFCT0L-MPARCODE-APLIC
               MOVE  GFCTED-DESC-MSG-ERRO
                                       TO      GFCT0M-TEXTO
               PERFORM  32000-TRATAR-ERRO
           END-IF.

           PERFORM  40100-TRATAR-RETORNO-CONSULTA.

           PERFORM  40200-RETORNAR-PARA-WEB.

      *----------------------------------------------------------------*
       40000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       40100-TRATAR-RETORNO-CONSULTA   SECTION.
      *----------------------------------------------------------------*

           IF  GFCTED-FIM              EQUAL   'S'
               MOVE  'MSGF'            TO      GFCT0L-TPOMSG-WEB
           ELSE
               MOVE  'MSGI'            TO      GFCT0L-TPOMSG-WEB
               MOVE GFCTED-PONTEIRO    TO GFCTEC-PONTEIRO
               MOVE GFCTED-FIM         TO GFCTEC-FIM
               IF  WRK-NRO-MSGI-H      GREATER 2
                   MOVE  'RSTA'        TO      GFCT0L-TPOMSG-WEB
               ELSE
                   ADD   1             TO      WRK-NRO-MSGI-H
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL   'RSTA'  OR  'MSGF'
               MOVE  ZEROS             TO      WRK-NRO-MSGI-H
           END-IF.

           MOVE  '00'                  TO      GFCT0L-MPARCODE-APLIC.
           MOVE  GFCT0L-TPOMSG-WEB     TO      WRK-FUNCAO.

      *----------------------------------------------------------------*
       40100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       40200-RETORNAR-PARA-WEB         SECTION.
      *----------------------------------------------------------------*

           MOVE  GFCTED-SAIDA          TO      GFCT0L-ENTRADA.

           MOVE  256                   TO      GFCT3O-LL.
           MOVE  GFCTED-FUNCAO         TO      GFCT3O-FUNCAO.
           MOVE  GFCTEC-DADOS-RST      TO      GFCT3O-DADOS-RST.

           IF  WRK-FUNCAO              EQUAL   'RSTA'
               CALL  'SBAT3000'        USING   WRK-FUNCAO
                                               IO-PCB
                                               ALT-PCB
                                               MCA-ISDPA
                                               GFCT0L-ENTRADA
                                               GFCT3O-AREA-RESTART
           ELSE
               CALL  'SBAT3000'        USING   WRK-FUNCAO
                                               IO-PCB
                                               ALT-PCB
                                               MCA-ISDPA
                                               GFCT0L-ENTRADA
           END-IF.

           IF  MPA-RCODE-API       NOT EQUAL   SPACES
               IF  WRK-FUNCAO          EQUAL   'MSGI'
                   MOVE  WRK-MSG02     TO      GFCT0M-TEXTO
               ELSE
                   IF  WRK-FUNCAO      EQUAL   'MSGF'
                       MOVE  WRK-MSG03 TO      GFCT0M-TEXTO
                   ELSE
                       MOVE  WRK-MSG04 TO      GFCT0M-TEXTO
                   END-IF
               END-IF
               MOVE  'APL'             TO      GFCT0M-TIPO-ACESSO
               MOVE  'SBAT3000'        TO      GFCT0M-TRANSACAO
               MOVE  '02'              TO      GFCT0L-MPARCODE-APLIC
               PERFORM  32000-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       40200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
