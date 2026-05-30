      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM1ABC.
       AUTHOR.     LUCIANDRA SILVEIRA.
      *================================================================*
      *                       WIPRO DO BRASIL                          *
      *----------------------------------------------------------------*
      *    PROGRAMA....: DCOM1ABC                                      *
      *    PROGRAMADOR.: LUCIANDRA SILVEIRA - WIPRO                    *
      *    ANALISTA....: RICARDO JAMMAL     - G439                     *
      *    DATA........: 05/08/2022                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....: COORDENADOR PARA VERIFICAR SE CLIENTE POSSUI  *
      *                  CONTRATO LIMITE ATIVO.                        *
      *----------------------------------------------------------------*
      *    BOOKS FUNCIONAIS:                                           *
      *    DCOMW000 - BOOK DE ERROS PARA COMUNICACAO ENTRE MODULOS     *
      *    DCOMWABE - AREA DE INTERFACE COM FRWK PARA DADOS DE ENTRADA *
      *    DCOMWABI - BOOK DE INTERFACE ENTRE COORDENADOR E FUNCIONAL  *
      *    DCOMWABS - AREA DE INTERFACE COM FRWK PARA DADOS DE SAIDA   *
      *                                                                *
      *    BOOKS ARQUITETURAIS:                                        *
      *    I#FRWKAQ COMMAREA ENTRE PROGRAMA/FRAMEWORK                  *
      *    I#FRWK04 COMMAREA ENTRE PROGRAMA/FRWK1200(GAM)              *
      *    FRWKWAAA COMMAREA ENTRE PROGRAMA/FRWK1200 - DADOS SESSAO    *
      *    I#FRWKGE COMMAREA FRWK1999 (LOG DE ERRO)                    *
      *    I#FRWKMD COMMAREA FRWK1999 (LOG DE ERROS MODULO)            *
      *    I#FRWKCI COMMAREA FRWK1999 (LOG DE ERROS CICS)              *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    FRWK1999 - PROCEDIMENTOS PARA GRAVACAO DE LOGS DE ERRO      *
      *    FRWK1200 - MODULO DE ACESSO AO GAM                          *
      *    DCOM3ABC - FUNCIONAL VERIFICAR SE CLIENTE POSSUI CONTRATO   *
      *               LIMITE ATIVO PARA O TIPO DE DESCONTO INFORMADO.  *
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
       77  FILLER                      PIC  X(050)         VALUE
           '*** INICIO DA WORKING DCOM1ABC ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-PROGRAMA             PIC  X(008)         VALUE
              'DCOM1ABC'.
           05 WRK-DCOM3ABC             PIC  X(008)         VALUE
              'DCOM3ABC'.
           05 WRK-FRWK1200             PIC  X(008)         VALUE
              'FRWK1200'.
           05 WRK-FRWK1999             PIC  X(008)         VALUE
              'FRWK1999'.
           05 WRK-MODULO               PIC  X(008)         VALUE SPACES.


           05 WRK-CAUTEN-SEGRC.
              10 WRK-CAUTEN-SEGRC-1    PIC  9(002)         VALUE ZEROS.
              10 WRK-CAUTEN-SEGRC-2    PIC  9(001)         VALUE ZEROS.
              10 WRK-CAUTEN-SEGRC-3.
                 15 WRK-CAUTEN-SEGRC-3-N
                                       PIC  9(006)         VALUE ZEROS.
           05 WRK-CAUTEN-SEGRC-N       REDEFINES
              WRK-CAUTEN-SEGRC         PIC  9(009).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(060)         VALUE
           '*** AREA DE COMUNICACAO DO FRWK1999 (LOG DE ERRO) ***'.
      *----------------------------------------------------------------*

       01  WRK-AREA-ERRO.
           COPY 'I#FRWKGE'.
           05 WRK-BLOCO-INFO-ERRO.
              10 WRK-CHAR-INFO-ERRO    PIC  X(001)
                                       OCCURS 000 TO 00526 TIMES
                                       DEPENDING  ON FRWKGHEA-TAM-DADOS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(060)         VALUE
           '*** AREA DE TRATAMENTO DE ERRO MODULO (API-ERRO) ***'.
      *----------------------------------------------------------------*

       01  WRK-AREA-ERRO-MODULO.
           COPY 'I#FRWKMD'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(060)         VALUE
           '*** AREA DE TRATAMENTO DE ERRO CICS(API-ERRO) ***'.
      *----------------------------------------------------------------*

       01  WRK-AREA-ERRO-CICS.
           COPY 'I#FRWKCI'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(060)         VALUE
           '*** AREA DE TRATAMENTO DE INTERFACE (GAM) ***'.
      *----------------------------------------------------------------*

       01  WRK-AREA-FRWK1200.
           COPY 'I#FRWK04'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA BUSCAR DADOS DE SESSAO ***'.
      *----------------------------------------------------------------*

       01  WRK-AREA-SESSAO.
           COPY 'FRWKWAAA'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE INTERFACE PARA DADOS DE ENTRADA ***'.
      *----------------------------------------------------------------*

       01  WRK-AREA-ENTRADA.
           COPY 'DCOMWABE'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE INTERFACE PARA DADOS DE SAIDA ***'.
      *----------------------------------------------------------------*

       01  WRK-AREA-SAIDA.
           COPY 'DCOMWABS'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(060)         VALUE
           '*** AREA DE INTERFACE COM SERVICO FUNCIONAL ***'.
      *----------------------------------------------------------------*

       01  WRK-AREA-DCOM3ABC.
           COPY 'DCOMW000'.
           COPY 'DCOMWABI'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)  VALUE
           '*** FIM DA WORKING DCOM1ABC ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  DFHCOMMAREA.
           COPY 'I#FRWKAQ'.

      *================================================================*
       PROCEDURE DIVISION              USING DFHCOMMAREA.
      *================================================================*

      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL DO PROGRAMA.                               *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA INICIAL DO PROGRAMA                                  *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE '1000-INICIALIZAR'     TO FRWKGHEA-IDEN-PARAGRAFO.

           INITIALIZE FRWKGHEA-REGISTRO
                      FRWKGCIC-REGISTRO
                      FRWKGMOD-REGISTRO
                      FRWKGCIC-REGISTRO
                      FRWKGLAQ-BLOCO-RETORNO
                      FRWKEL04-BLOCO-RETORNO
                      DCOMW000-BLOCO-REGISTRO
                      DCOMWABE-BLOCO-ENTRADA.

           PERFORM 6000-OBTER-DADOS-ENTRADA.

           PERFORM 1100-CONSISTIR-DADOS-ENTRADA.

           PERFORM 6100-RECUPERAR-DADOS-SESSAO.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA CONSISTENCIA DOS DADOS DE ENTRADA.              *
      *----------------------------------------------------------------*
       1100-CONSISTIR-DADOS-ENTRADA    SECTION.
      *----------------------------------------------------------------*

           MOVE '1100-CONSISTIR-DADOS-ENTRADA'
                                       TO FRWKGHEA-IDEN-PARAGRAFO

           IF  DCOMWABE-CTERM          EQUAL SPACES
               MOVE 08                 TO FRWKGLAQ-COD-RETORNO
               MOVE '0010'             TO FRWKGLAQ-COD-ERRO
      *--> CODIGO DO CANAL INVALIDO
               MOVE 'DCOM0043'         TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  DCOMWABE-CTPO-DESC-COML    NOT EQUAL 004
               MOVE 08                 TO FRWKGLAQ-COD-RETORNO
               MOVE '0020'             TO FRWKGLAQ-COD-ERRO
      *--> CODIGO PARA TIPO DE DESCONTO INVALIDO
               MOVE 'DCOM0041'         TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  DCOMWABE-CBCO           NOT NUMERIC OR
               DCOMWABE-CBCO           EQUAL ZEROS
               MOVE 08                 TO FRWKGLAQ-COD-RETORNO
               MOVE '0030'             TO FRWKGLAQ-COD-ERRO
      *--> CODIGO DO BANCO INVALIDO
               MOVE 'DCOM0037'         TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  DCOMWABE-CAG-BCRIA      NOT NUMERIC OR
               DCOMWABE-CAG-BCRIA      EQUAL ZEROS
               MOVE 08                 TO FRWKGLAQ-COD-RETORNO
               MOVE '0040'             TO FRWKGLAQ-COD-ERRO
      *--> CODIGO DE AGENCIA INVALIDO
               MOVE 'DCOM0003'         TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  DCOMWABE-CCTA-BCRIA-CLI    NOT NUMERIC OR
               DCOMWABE-CCTA-BCRIA-CLI    EQUAL ZEROS
               MOVE 08                 TO FRWKGLAQ-COD-RETORNO
               MOVE '0050'             TO FRWKGLAQ-COD-ERRO
      *--> NUMERO DA CONTA CORRENTE INVALIDO
               MOVE 'DCOM0025'         TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

4S2511*    IF  DCOMWABE-CCNPJ-CPF      NOT NUMERIC OR
4S2511*        DCOMWABE-CCNPJ-CPF      EQUAL ZEROS
4S2511     IF  DCOMWABE-CCNPJ-CPF      EQUAL SPACES OR
4S2511         DCOMWABE-CCNPJ-CPF      EQUAL LOW-VALUES
                MOVE 08                 TO FRWKGLAQ-COD-RETORNO
                MOVE '0060'             TO FRWKGLAQ-COD-ERRO
      *--> CNPJ/CPF INVALIDO
                MOVE 'DCOM0023'         TO FRWKGLAQ-COD-MENSAGEM
                PERFORM 3000-FINALIZAR
            END-IF.

4S2511*    IF  DCOMWABE-CFLIAL-CNPJ    NOT NUMERIC
4S2511     IF  DCOMWABE-CFLIAL-CNPJ    EQUAL SPACES OR
4S2511         DCOMWABE-CFLIAL-CNPJ    EQUAL LOW-VALUES
                MOVE 08                 TO FRWKGLAQ-COD-RETORNO
                MOVE '0070'             TO FRWKGLAQ-COD-ERRO
      *--> CNPJ/CPF INVALIDO
                MOVE 'DCOM0023'         TO FRWKGLAQ-COD-MENSAGEM
                PERFORM 3000-FINALIZAR
            END-IF.

           IF  DCOMWABE-CCTRL-CNPJ-CPF    NOT NUMERIC
               MOVE 08                 TO FRWKGLAQ-COD-RETORNO
               MOVE '0080'             TO FRWKGLAQ-COD-ERRO
      *--> CNPJ/CPF INVALIDO
               MOVE 'DCOM0023'         TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA.                        *
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-ACESSAR-FUNCIONAL.

           PERFORM 6200-CRIAR-BLOCO-SAIDA.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FORMATAR O COPYBOOK DO SERVICO FUNCIONAL        *
      *----------------------------------------------------------------*
       2100-ACESSAR-FUNCIONAL          SECTION.
      *----------------------------------------------------------------*

           MOVE '2100-ACESSAR-FUNCIONAL'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
           MOVE DCOMWABE-BLOCO-ENTRADA TO DCOMWABI-BLOCO-ENTRADA.

           EXEC CICS LINK
                PROGRAM  (WRK-DCOM3ABC)
                COMMAREA (WRK-AREA-DCOM3ABC)
                LENGTH   (LENGTH OF WRK-AREA-DCOM3ABC)
                NOHANDLE
           END-EXEC

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
               MOVE '0060'             TO FRWKGLAQ-COD-ERRO
               PERFORM 9400-TRATAR-ERRO-CICS
           END-IF.

           EVALUATE DCOMW000-COD-RETORNO
                                       OF WRK-AREA-DCOM3ABC
               WHEN 00
                    MOVE DCOMWABI-BLOCO-SAIDA
                                       TO DCOMWABS-BLOCO-SAIDA

               WHEN 08
                    MOVE DCOMW000-COD-RETORNO
                                       OF WRK-AREA-DCOM3ABC
                                       TO FRWKGLAQ-COD-RETORNO
                    MOVE DCOMW000-COD-ERRO
                                       OF WRK-AREA-DCOM3ABC
                                       TO FRWKGLAQ-COD-ERRO
                    MOVE DCOMW000-COD-MENSAGEM
                                       OF WRK-AREA-DCOM3ABC
                                       TO FRWKGLAQ-COD-MENSAGEM
                    PERFORM 3000-FINALIZAR

               WHEN OTHER
                    MOVE 16            TO FRWKGLAQ-COD-RETORNO
                    MOVE '0050'        TO FRWKGLAQ-COD-ERRO
                    MOVE DCOMW000-COD-MENSAGEM
                                       OF WRK-AREA-DCOM3ABC
                                       TO FRWKGLAQ-COD-MENSAGEM
                    MOVE DCOMW000-BLOCO-RETORNO
                                       OF WRK-AREA-DCOM3ABC
                                       TO FRWKGLAQ-BLOCO-RETORNO
                    MOVE WRK-DCOM3ABC  TO WRK-MODULO
                    PERFORM 9300-TRATAR-ERRO-MODULO
           END-EVALUATE.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZAR O PROCESSAMENTO.                      *
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
                RETURN
           END-EXEC.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    EFETUAR CHAMADA AO MODULO FRWK1200 - GAM                    *
      *----------------------------------------------------------------*
       5000-ACESSAR-GAM-FRWK1200       SECTION.
      *----------------------------------------------------------------*

           MOVE '5000-ACESSAR-GAM-FRWK1200'
                                       TO FRWKGHEA-IDEN-PARAGRAFO.

           INITIALIZE FRWKGHEA-REGISTRO
                      FRWKGCIC-REGISTRO.

           EXEC CICS LINK
                PROGRAM  (WRK-FRWK1200)
                COMMAREA (WRK-AREA-FRWK1200)
                LENGTH   (LENGTH OF WRK-AREA-FRWK1200)
                NOHANDLE
           END-EXEC.

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
               MOVE '0080'             TO FRWKGLAQ-COD-ERRO
               PERFORM 9400-TRATAR-ERRO-CICS
           END-IF

           IF (FRWKEL04-COD-RETORNO    NOT EQUAL ZEROS)
               MOVE 16                 TO FRWKGLAQ-COD-RETORNO
               MOVE '0090'             TO FRWKGLAQ-COD-ERRO
               MOVE FRWKEL04-COD-MENSAGEM
                                       TO FRWKGLAQ-COD-MENSAGEM
               MOVE FRWKEL04-BLOCO-RETORNO
                                       TO FRWKGLAQ-BLOCO-RETORNO
               MOVE WRK-FRWK1200       TO WRK-MODULO
               PERFORM 9300-TRATAR-ERRO-MODULO
           END-IF.

      *----------------------------------------------------------------*
       5000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA OBTER OS DADOS DE ENTRADA.                      *
      *----------------------------------------------------------------*
       6000-OBTER-DADOS-ENTRADA        SECTION.
      *----------------------------------------------------------------*

           MOVE '6000-OBTER-DADOS-ENTRADA'
                                       TO FRWKGHEA-IDEN-PARAGRAFO.

           INITIALIZE FRWKEL04-BLOCO-ENTRADA
                      FRWKEL04-BLOCO-RETORNO

           SET  FRWKEL04-FUN-CONSULTAR TO TRUE.

           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO FRWKEL04-IDENT-AREA-GLOBAL.
           MOVE DCOMWABE-COD-LAYOUT    TO FRWKEL04-ID-BLOCO.
           MOVE LENGTH                 OF WRK-AREA-ENTRADA
                                       TO FRWKEL04-TAM-BLOCO.

           PERFORM 5000-ACESSAR-GAM-FRWK1200.

           MOVE FRWKEL04-BUFFER (1:FRWKEL04-TAM-BLOCO)
             TO WRK-AREA-ENTRADA(1:FRWKEL04-TAM-BLOCO).

      *----------------------------------------------------------------*
       6000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA RECUPERAR DADOS DE SESSAO.                      *
      *----------------------------------------------------------------*
       6100-RECUPERAR-DADOS-SESSAO     SECTION.
      *----------------------------------------------------------------*

           MOVE '6100-RECUPERAR-DADOS-SESSAO'
                                       TO FRWKGHEA-IDEN-PARAGRAFO.

           INITIALIZE FRWKEL04-BLOCO-ENTRADA
                      FRWKEL04-BLOCO-RETORNO

           SET FRWKEL04-FUN-CONSULTAR  TO TRUE.

           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO FRWKEL04-IDENT-AREA-GLOBAL.
           MOVE 'FRWKEL83'             TO FRWKEL04-ID-BLOCO.
           MOVE LENGTH                 OF WRK-AREA-SESSAO
                                       TO FRWKEL04-TAM-BLOCO.

           PERFORM 5000-ACESSAR-GAM-FRWK1200.

           MOVE FRWKEL04-BUFFER        (1:FRWKEL04-TAM-BLOCO)
                                       TO WRK-AREA-SESSAO.

           MOVE FRWKWAAA-CCANAL        TO DCOMWABI-T-CCANAL.

           EVALUATE FRWKWAAA-CCANAL
               WHEN 035
                    MOVE 'NETEMPR '    TO DCOMWABI-E-CTERM

               WHEN 063
                    MOVE 'MOBILE  '    TO DCOMWABI-E-CTERM
           END-EVALUATE.

           IF  DCOMWABI-E-CTERM        EQUAL 'NETEMPR '
                                          OR 'MOBILE  '
               MOVE 1                  TO WRK-CAUTEN-SEGRC-N
           ELSE
               MOVE ZEROS              TO WRK-CAUTEN-SEGRC-N
               EVALUATE  FRWKWAAA-CAUTEN-SEGRC(1:1)
                   WHEN 'A'
                         MOVE 1        TO WRK-CAUTEN-SEGRC-2
                   WHEN 'B'
                         MOVE 2        TO WRK-CAUTEN-SEGRC-2
                   WHEN 'C'
                         MOVE 3        TO WRK-CAUTEN-SEGRC-2
                   WHEN 'D'
                         MOVE 4        TO WRK-CAUTEN-SEGRC-2
                   WHEN 'E'
                         MOVE 5        TO WRK-CAUTEN-SEGRC-2
                   WHEN 'F'
                         MOVE 6        TO WRK-CAUTEN-SEGRC-2
                   WHEN 'G'
                         MOVE 7        TO WRK-CAUTEN-SEGRC-2
                   WHEN 'H'
                         MOVE 8        TO WRK-CAUTEN-SEGRC-2
                   WHEN 'I'
                         MOVE 9        TO WRK-CAUTEN-SEGRC-2
               END-EVALUATE
               MOVE FRWKWAAA-CAUTEN-SEGRC(2:6)
                                       TO WRK-CAUTEN-SEGRC-3
           END-IF.

           MOVE WRK-CAUTEN-SEGRC       TO DCOMWABI-T-CUSUAR-MANUT.

      *----------------------------------------------------------------*
       6100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA CRIACAO DA AREA PARA ARMAZENAR AS INFORMACOES DE   *
      * DE SAIDA.                                                      *
      *----------------------------------------------------------------*
       6200-CRIAR-BLOCO-SAIDA          SECTION.
      *----------------------------------------------------------------*

           MOVE '6300-CRIAR-BLOCO-SAIDA'
                                       TO FRWKGHEA-IDEN-PARAGRAFO.

           INITIALIZE FRWKEL04-BLOCO-ENTRADA
                      FRWKEL04-BLOCO-RETORNO.

           SET FRWKEL04-FUN-CRIAR      TO TRUE
           SET FRWKEL04-PERM-CONSULTAR TO TRUE
           SET FRWKEL04-IND-BL-SAIDA   TO TRUE

           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO FRWKEL04-IDENT-AREA-GLOBAL
           MOVE DCOMWABS-COD-LAYOUT    TO FRWKEL04-ID-BLOCO

           MOVE LENGTH                 OF WRK-AREA-SAIDA
                                       TO FRWKEL04-TAM-BLOCO
                                          DCOMWABS-TAM-LAYOUT.
           MOVE WRK-AREA-SAIDA         (1:FRWKEL04-TAM-BLOCO)
                                       TO FRWKEL04-BUFFER
                                       (1:FRWKEL04-TAM-BLOCO)

           PERFORM 5000-ACESSAR-GAM-FRWK1200.

           MOVE DCOMW000-BLOCO-RETORNO OF WRK-AREA-DCOM3ABC
                                       TO FRWKGLAQ-BLOCO-RETORNO.

      *----------------------------------------------------------------*
       6200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA GERAR LOG DE ERRO DE MODULO                     *
      *----------------------------------------------------------------*
       9300-TRATAR-ERRO-MODULO         SECTION.
      *----------------------------------------------------------------*

           SET ERRO-MODULO             TO TRUE.

           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA.
           MOVE WRK-MODULO             TO FRWKGMOD-NOME-MODULO.
           MOVE FRWKGMOD-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS
           MOVE FRWKGLAQ-BLOCO-RETORNO TO FRWKGMOD-BLOCO-RETORNO.
           MOVE WRK-AREA-ERRO-MODULO   TO WRK-BLOCO-INFO-ERRO
                                         (1:FRWKGHEA-TAM-DADOS).

           PERFORM 9999-CHAMAR-API-ERROS.

      *----------------------------------------------------------------*
       9300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO CICS.                        *
      *----------------------------------------------------------------*
       9400-TRATAR-ERRO-CICS           SECTION.
      *----------------------------------------------------------------*

           SET ERRO-CICS               TO TRUE.

           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA.
           MOVE FRWKGCIC-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS.

           MOVE 16                     TO FRWKGLAQ-COD-RETORNO.
      *--> ERRO CICS
           MOVE 'DCOM0010'             TO FRWKGLAQ-COD-MENSAGEM.
           MOVE EIBFN                  TO FRWKGCIC-EIBFN.
           MOVE EIBRCODE               TO FRWKGCIC-EIBRCODE.
           MOVE EIBRSRCE               TO FRWKGCIC-EIBRSRCE.
           MOVE EIBRESP                TO FRWKGCIC-EIBRESP.
           MOVE EIBRESP2               TO FRWKGCIC-EIBRESP2.
           MOVE EIBTASKN               TO FRWKGCIC-EIBTASKN.
           MOVE WRK-AREA-ERRO-CICS     TO WRK-BLOCO-INFO-ERRO
                                         (1:FRWKGHEA-TAM-DADOS).

           PERFORM 9999-CHAMAR-API-ERROS.

      *----------------------------------------------------------------*
       9400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA GRAVAR LOGS DE ERRO GERADOS DURANTE O PROCESSAMENTO*
      *----------------------------------------------------------------*
       9999-CHAMAR-API-ERROS           SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
                LINK
                PROGRAM  (WRK-FRWK1999)
                COMMAREA (WRK-AREA-ERRO)
                LENGTH   (LENGTH OF WRK-AREA-ERRO)
                NOHANDLE
           END-EXEC

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
               CONTINUE
           END-IF

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
