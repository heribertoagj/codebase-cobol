      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.                     CTEC1ASL.
       AUTHOR.                         HERIBERTO GIANNASI.

      *================================================================*
      *                        S T E F A N I N I                       *
      *================================================================*
      *    PROGRAMA....:  CTEC1ASL                                     *
      *    FLUXO.......:  CTECIAET                                     *
      *    ANALISTA....:  HERIBERTO GIANNASI               - STEFANINI *
      *    DATA........:  10/2024                                      *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  PROGRAMA COORDENADOR QUE ATENDE A FUNCAO     *
      *                   CTEC.SI.LISTAR.PARÂMETRO.BLOQUEIO.JUDICIAL   *  
      *----------------------------------------------------------------*
      *    BOOK'S FUNCIONAIS...:                                       *
      *    CTECW00I - AREA COMUM PARA TRATAMENTO DE ERROS              *
      *    CTECWDHE - AREA PARA DADOS DE ENTRADA                       *
      *    CTECWDHS - AREA PARA DADOS DE SAIDA                         *
      *    CTECWDHI - AREA DE INTERFACE COM PROGRAMA FUNCIONAL         *
      *    CTECWDHP - AREA PARA DADOS DE RESTART (PAGINACAO)           *
      *                                                                *
      *    BOOK'S ARQUITETURAIS:                                       *
      *    I#FRWKAQ - AREA DE COMUNICACAO DA LINKAGE                   *
      *    I#FRWK04 - AREA DE COMUNICACAO COM O GAM                    *
      *    FRWKWAAA - AREA DE ACESSO PARA DADOS DA SESSAO              *
      *    I#FRWKGE - TRATAMENTO DE ERROS - GERAL                      *
      *    I#FRWKCI - TRATAMENTO DE ERROS - CICS                       *
      *    I#FRWKMD - TRATAMENTO DE ERROS - MODULO                     *
      *    I#FRWKME - COMMAREA PARA GRAVACAO DE MENSAGENS VARIAVEIS    *
      *----------------------------------------------------------------*
      *    MODULOS.............:                                       *
      *    FRWK1200 - GERENCIADOR DO GAM  - BOOK I#FRWK04              *
      *    FRWK1999 - TRATAMENTO DE ERROS - BOOK I#FRWKGE              *
      *    CTEC3ASL - LISTAR PARAMETROS BLOQUEIO JUDICIAL              *
      *================================================================*
      *================================================================*
       ENVIRONMENT DIVISION.
      *================================================================*
      *----------------------------------------------------------------*
       CONFIGURATION SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *================================================================*
       DATA DIVISION.
      *================================================================*
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       77  FILLER                      PIC X(050) VALUE
           '*** INICIO DA WORKING CTEC1ASL ***'.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       77  FILLER                      PIC X(050) VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*
       77  WRK-PROGRAMA                PIC X(008) VALUE
           'CTEC1ASL'.

       77  WRK-PARAGRAFO               PIC X(030) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050) VALUE
           'AREA DE COMUNICACAO COM O GAM'.
      *----------------------------------------------------------------*
       01  WRK-FRWK1200                PIC X(008) VALUE
           'FRWK1200'.

       01  WRK-AREA-GAM.
           COPY 'I#FRWK04'.

       01  WRK-AREA-SESSAO.
           COPY FRWKWAAA.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050) VALUE
           'AREA DE COMUNICACAO DO PROGRAMA FRWK1999'.
      *----------------------------------------------------------------*
       01  WRK-FRWK1999                PIC X(008) VALUE
           'FRWK1999'.

       01  WRK-AREA-ERRO.
           COPY 'I#FRWKGE'.
           05 WRK-BLOCO-INFO-ERRO.
              10 WRK-CHAR-INFO-ERRO    PIC X(001)
                                       OCCURS 0 TO 1000 TIMES
                                       DEPENDING ON FRWKGHEA-TAM-DADOS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050) VALUE
           'AREA DE TRATAMENTO DE ERRO MODULO'.
      *----------------------------------------------------------------*
       01  WRK-AREA-ERRO-MODULO.
           COPY 'I#FRWKMD'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050) VALUE
           'AREA DE TRATAMENTO DE ERRO CICS'.
      *----------------------------------------------------------------*
       01  WRK-AREA-ERRO-CICS.
           COPY 'I#FRWKCI'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050) VALUE
           'AREA PARA DADOS DE ENTRADA'.
      *----------------------------------------------------------------*

       01  WRK-AREA-ENTRADA.
           COPY 'CTECWDHE'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050) VALUE
           'AREA PARA DADOS DE SAIDA'.
      *----------------------------------------------------------------*
       01  WRK-AREA-SAIDA.
           COPY 'CTECWDHS'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050) VALUE
           'AREA DO SERVICO FUNCIONAL'.
      *----------------------------------------------------------------*
       01  WRK-CTEC3ASL                PIC X(008) VALUE
           'CTEC3ASL'.

       01  WRK-AREA-FUNCIONAL.
           COPY 'CTECW00I'.
           COPY 'CTECWDHI'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050) VALUE
           'AREA PARA DADOS DE PAGINACAO'.
      *----------------------------------------------------------------*
       01  WRK-AREA-PAGINACAO.
           COPY 'CTECWDHP'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050) VALUE
           '*** FIM DA WORKING CTEC1ASL ***'.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       LINKAGE SECTION.
      *----------------------------------------------------------------*
       01  DFHCOMMAREA.
           COPY 'I#FRWKAQ'.

      *================================================================*
       PROCEDURE                       DIVISION  USING DFHCOMMAREA.
      *================================================================*
      *----------------------------------------------------------------*
       0000-PRINCIPAL                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*
           MOVE '1000-INICIALIZAR'     TO WRK-PARAGRAFO.

           INITIALIZE FRWKGHEA-REGISTRO
                      FRWKGCIC-REGISTRO
                      FRWKGMOD-REGISTRO.

           PERFORM 1100-RECUPERAR-DADOS-ENTRADA.

           PERFORM 1200-RECUPERAR-DADOS-SESSAO.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1100-RECUPERAR-DADOS-ENTRADA    SECTION.
      *----------------------------------------------------------------*

           MOVE '1100-RECUPERAR-DADOS-ENTRADA'
                                       TO WRK-PARAGRAFO.

           INITIALIZE  FRWKEL04-BLOCO-ENTRADA
                       FRWKEL04-BLOCO-RETORNO
                       FRWKEL04-BLOCO-ENTRADA-SAIDA.

           SET FRWKEL04-FUN-CONSULTAR  TO TRUE.

           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO FRWKEL04-IDENT-AREA-GLOBAL.
           MOVE CTECWDHE-COD-LAYOUT    TO FRWKEL04-ID-BLOCO.
           MOVE LENGTH OF WRK-AREA-ENTRADA
                                       TO FRWKEL04-TAM-BLOCO.

           PERFORM 5000-ACESSAR-GAM.

           MOVE FRWKEL04-BUFFER(1:FRWKEL04-TAM-BLOCO)
                                       TO WRK-AREA-ENTRADA.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1200-RECUPERAR-DADOS-SESSAO     SECTION.
      *----------------------------------------------------------------*
           MOVE '1200-RECUPERAR-DADOS-SESSAO'
                                       TO WRK-PARAGRAFO.

           INITIALIZE  FRWKEL04-BLOCO-ENTRADA
                       FRWKEL04-BLOCO-RETORNO
                       FRWKEL04-BLOCO-ENTRADA-SAIDA.

           SET FRWKEL04-FUN-CONSULTAR  TO TRUE.

           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO FRWKEL04-IDENT-AREA-GLOBAL.
           MOVE 'FRWKEL83'             TO FRWKEL04-ID-BLOCO.

           MOVE LENGTH                 OF WRK-AREA-SESSAO
                                       TO FRWKEL04-TAM-BLOCO.

           PERFORM 5000-ACESSAR-GAM.

           MOVE FRWKEL04-BUFFER(1:FRWKEL04-TAM-BLOCO)
                                       TO WRK-AREA-SESSAO.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*
           MOVE '2000-PROCESSAR'       TO WRK-PARAGRAFO.

           EVALUATE TRUE
               WHEN FRWKWAAA-CONSULTA-INICIAL
                    PERFORM 2100-CONSULTA-INICIAL

               WHEN FRWKWAAA-BLOCO-SEGUINTE
               WHEN FRWKWAAA-BLOCO-ANTERIOR
               WHEN FRWKWAAA-PRIMEIRO-BLOCO
               WHEN FRWKWAAA-ULTIMO-BLOCO
                    PERFORM 2200-PAGINACAO

               WHEN OTHER
                    SET  ERRO-LIVRE    TO TRUE
                    MOVE 16            TO FRWKGLAQ-COD-RETORNO
                    MOVE '0030'        TO FRWKGLAQ-COD-ERRO
                    MOVE 'CTEC0041'    TO FRWKGLAQ-COD-MENSAGEM
                    PERFORM 9998-TRATAR-ERRO
           END-EVALUATE.

           MOVE CTECW00I-COD-RETORNO   TO FRWKGLAQ-COD-RETORNO.
           MOVE CTECW00I-COD-ERRO      TO FRWKGLAQ-COD-ERRO.
           MOVE CTECW00I-COD-MENSAGEM  TO FRWKGLAQ-COD-MENSAGEM.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2100-CONSULTA-INICIAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 2110-VALORIZAR-AREA-FUNCIONAL.

           PERFORM 5100-ACESSAR-SERVICO-FUNCIONAL.

           PERFORM 2120-VALORIZAR-BLOCO-SAIDA.

           PERFORM 6100-GRAVAR-BLOCO-SAIDA.

      *---- SE HOUVER MAIS DADOS A LISTAR E FOR PRIMEIRA EXECUCAO
      *---- CRIA BLOCO NO GAM, SENAO, ATUALIZA BLOCO

           IF (CTECW00I-COD-RETORNO    EQUAL 01)
               IF (FRWKWAAA-CONSULTA-INICIAL)
                   PERFORM 6200-CRIAR-GAM-PERSISTENTE
               ELSE
                   PERFORM 6300-ATUALIZAR-GAM-PERSISTENTE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2110-VALORIZAR-AREA-FUNCIONAL   SECTION.
      *----------------------------------------------------------------*

           INITIALIZE CTECW00I-BLOCO-RETORNO
                      CTECWDHI-BLOCO-PAGINACAO.

           MOVE FRWKWAAA-SOLIC-MAIS-DADOS
                                       TO CTECWDHI-IND-PAGINACAO.         
           MOVE CTECWDHE-BLOCO-ENTRADA TO CTECWDHI-BLOCO-ENTRADA.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2120-VALORIZAR-BLOCO-SAIDA      SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2200-PAGINACAO                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 6400-CONSULTAR-GAM-PERSISTENTE.

           PERFORM 2110-VALORIZAR-AREA-FUNCIONAL.

           PERFORM 2220-CARREGAR-CHAVE.

           PERFORM 5100-ACESSAR-SERVICO-FUNCIONAL.

           PERFORM 2120-VALORIZAR-BLOCO-SAIDA.

           PERFORM 6100-GRAVAR-BLOCO-SAIDA.

           PERFORM 6300-ATUALIZAR-GAM-PERSISTENTE.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2220-CARREGAR-CHAVE             SECTION.
      *----------------------------------------------------------------*

           MOVE CTECWDHP-PRI-CHAVE     TO CTECWDHI-PRI-CHAVE.
                                       
           MOVE CTECWDHP-ULT-CHAVE     TO CTECWDHI-ULT-CHAVE.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
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
       5000-ACESSAR-GAM                SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
                LINK
                PROGRAM  (WRK-FRWK1200)
                COMMAREA (WRK-AREA-GAM)
                LENGTH   (LENGTH OF WRK-AREA-GAM)
                NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               SET ERRO-CICS           TO TRUE
               MOVE 16                 TO FRWKGLAQ-COD-RETORNO
               MOVE '0040'             TO FRWKGLAQ-COD-ERRO
               MOVE 'CTEC0030'         TO FRWKGLAQ-COD-MENSAGEM
               MOVE '5000-ACESSAR-GAM' TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9998-TRATAR-ERRO
           END-IF.

           IF (FRWKEL04-COD-RETORNO    NOT EQUAL ZEROS)
               SET ERRO-MODULO         TO TRUE
               MOVE 12                 TO FRWKGLAQ-COD-RETORNO
               MOVE '0050'             TO FRWKGLAQ-COD-ERRO
               MOVE FRWKEL04-COD-MENSAGEM
                                       TO FRWKGLAQ-COD-MENSAGEM
               MOVE FRWKEL04-COD-RETORNO
                                       TO FRWKGMOD-COD-RETORNO
               MOVE FRWKEL04-COD-ERRO  TO FRWKGMOD-COD-ERRO
               MOVE FRWKEL04-COD-MENSAGEM
                                       TO FRWKGMOD-COD-MENSAGEM
               MOVE WRK-FRWK1200       TO FRWKGMOD-NOME-MODULO
               PERFORM 9998-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       5000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       5100-ACESSAR-SERVICO-FUNCIONAL  SECTION.
      *----------------------------------------------------------------*

           MOVE '5100-ACESSAR-SERVICO-FUNCIONAL'
                                       TO WRK-PARAGRAFO.

           EXEC CICS
                LINK
                PROGRAM  (WRK-CTEC3ASL)
                COMMAREA (WRK-AREA-FUNCIONAL)
                LENGTH   (LENGTH OF WRK-AREA-FUNCIONAL)
                NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               SET ERRO-CICS           TO TRUE
               MOVE 16                 TO FRWKGLAQ-COD-RETORNO
               MOVE 'CTEC0030'         TO FRWKGLAQ-COD-MENSAGEM
               MOVE '0060'             TO FRWKGLAQ-COD-ERRO
               MOVE '5100-ACESSAR-SERVICO-FUNCIONAL'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9998-TRATAR-ERRO
           END-IF.

           EVALUATE CTECW00I-COD-RETORNO
             WHEN 00
             WHEN 01
               MOVE CTECW00I-BLOCO-RETORNO                              
                                       TO FRWKGLAQ-BLOCO-RETORNO        
             WHEN 08
               MOVE CTECW00I-COD-RETORNO
                                       TO FRWKGLAQ-COD-RETORNO
               MOVE CTECW00I-COD-ERRO  TO FRWKGLAQ-COD-ERRO
               MOVE CTECW00I-COD-MENSAGEM
                                       TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
             WHEN OTHER
               SET ERRO-MODULO         TO TRUE
               MOVE 16                 TO FRWKGLAQ-COD-RETORNO
               MOVE CTECW00I-COD-ERRO  TO FRWKGLAQ-COD-ERRO
               MOVE CTECW00I-COD-MENSAGEM
                                       TO FRWKGLAQ-COD-MENSAGEM
               MOVE WRK-CTEC3ASL       TO FRWKGMOD-NOME-MODULO
               MOVE CTECW00I-BLOCO-RETORNO(1:14)
                                       TO FRWKGLAQ-BLOCO-RETORNO
                                          FRWKGMOD-BLOCO-RETORNO
               MOVE '5100-ACESSAR-SERVICO-FUNCIONAL'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9998-TRATAR-ERRO
           END-EVALUATE.

      *----------------------------------------------------------------*
       5100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       6100-GRAVAR-BLOCO-SAIDA         SECTION.
      *----------------------------------------------------------------*
           MOVE '6100-GRAVAR-BLOCO-SAIDA'
                                       TO WRK-PARAGRAFO.

           INITIALIZE FRWKEL04-BLOCO-ENTRADA
                      FRWKEL04-BLOCO-RETORNO
                      FRWKEL04-BLOCO-ENTRADA-SAIDA.

           SET FRWKEL04-FUN-CRIAR      TO TRUE.
           SET FRWKEL04-PERM-ATUALIZAR TO TRUE.
           SET FRWKEL04-IND-BL-SAIDA   TO TRUE.

           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO FRWKEL04-IDENT-AREA-GLOBAL.
           MOVE CTECWDHS-COD-LAYOUT    TO FRWKEL04-ID-BLOCO.
           MOVE CTECWDHS-TAM-LAYOUT    TO FRWKEL04-TAM-BLOCO.
           MOVE WRK-AREA-SAIDA         TO FRWKEL04-BUFFER
                                          (1:FRWKEL04-TAM-BLOCO).

           PERFORM 5000-ACESSAR-GAM.

      *----------------------------------------------------------------*
       6100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       6200-CRIAR-GAM-PERSISTENTE      SECTION.
      *----------------------------------------------------------------*
           MOVE '6200-CRIAR-GAM-PERSISTENTE'
                                       TO WRK-PARAGRAFO.

           INITIALIZE FRWKEL04-BLOCO-ENTRADA
                      FRWKEL04-BLOCO-RETORNO
                      FRWKEL04-BLOCO-ENTRADA-SAIDA.

           SET FRWKEL04-FUN-CRIAR      TO TRUE.
           SET FRWKEL04-PERM-RESTRITO  TO TRUE.
           SET FRWKEL04-IND-BL-PERS    TO TRUE.

           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO FRWKEL04-IDENT-AREA-GLOBAL.
           MOVE CTECWDHP-COD-LAYOUT    TO FRWKEL04-ID-BLOCO.
           MOVE CTECWDHP-TAM-LAYOUT    TO FRWKEL04-TAM-BLOCO.

           MOVE CTECWDHI-IND-PAGINACAO
                                       TO CTECWDHP-IND-PAGINACAO.

           MOVE CTECWDHI-BLOCO-PAGINACAO
                                       TO CTECWDHP-BLOCO-PAGINACAO.

           MOVE WRK-AREA-PAGINACAO      TO FRWKEL04-BUFFER
                                          (1:FRWKEL04-TAM-BLOCO).

           PERFORM 5000-ACESSAR-GAM.

      *----------------------------------------------------------------*
       6200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       6300-ATUALIZAR-GAM-PERSISTENTE  SECTION.
      *----------------------------------------------------------------*
           MOVE '6300-ATUALIZAR-GAM-PERSISTENTE'
                                       TO WRK-PARAGRAFO.

           INITIALIZE FRWKEL04-BLOCO-ENTRADA
                      FRWKEL04-BLOCO-RETORNO
                      FRWKEL04-BLOCO-ENTRADA-SAIDA.

           SET FRWKEL04-FUN-ATUALIZAR  TO TRUE.
           SET FRWKEL04-PERM-ATUALIZAR TO TRUE.
           SET FRWKEL04-IND-BL-PERS    TO TRUE.

           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO FRWKEL04-IDENT-AREA-GLOBAL.
           MOVE CTECWDHP-COD-LAYOUT    TO FRWKEL04-ID-BLOCO.
           MOVE CTECWDHP-TAM-LAYOUT    TO FRWKEL04-TAM-BLOCO.

           MOVE CTECWDHI-IND-PAGINACAO
                                       TO CTECWDHP-IND-PAGINACAO.

           MOVE CTECWDHI-BLOCO-PAGINACAO
                                       TO CTECWDHP-BLOCO-PAGINACAO.

           MOVE WRK-AREA-PAGINACAO     TO FRWKEL04-BUFFER.

           PERFORM 5000-ACESSAR-GAM.

      *----------------------------------------------------------------*
       6300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       6400-CONSULTAR-GAM-PERSISTENTE  SECTION.
      *----------------------------------------------------------------*
           MOVE '6400-CONSULTAR-GAM-PERSISTENTE'
                                       TO WRK-PARAGRAFO.

           INITIALIZE FRWKEL04-BLOCO-ENTRADA
                      FRWKEL04-BLOCO-RETORNO
                      FRWKEL04-BLOCO-ENTRADA-SAIDA.

           SET FRWKEL04-FUN-CONSULTAR  TO TRUE.

           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO FRWKEL04-IDENT-AREA-GLOBAL.
           MOVE CTECWDHP-COD-LAYOUT    TO FRWKEL04-ID-BLOCO.
           MOVE CTECWDHP-TAM-LAYOUT    TO FRWKEL04-TAM-BLOCO.

           PERFORM 5000-ACESSAR-GAM.

           MOVE FRWKEL04-BUFFER(1:FRWKEL04-TAM-BLOCO)
                                       TO WRK-AREA-PAGINACAO.

      *----------------------------------------------------------------*
       6400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       9998-TRATAR-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA.
           MOVE WRK-PARAGRAFO          TO FRWKGHEA-IDEN-PARAGRAFO.

           EVALUATE TRUE

               WHEN ERRO-CICS
                   MOVE FRWKGCIC-TAM-LAYOUT
                                       TO FRWKGHEA-TAM-DADOS
                   MOVE EIBFN          TO FRWKGCIC-EIBFN
                   MOVE EIBRCODE       TO FRWKGCIC-EIBRCODE
                   MOVE EIBRSRCE       TO FRWKGCIC-EIBRSRCE
                   MOVE EIBRESP        TO FRWKGCIC-EIBRESP
                   MOVE EIBRESP2       TO FRWKGCIC-EIBRESP2
                   MOVE EIBTASKN       TO FRWKGCIC-EIBTASKN
                   MOVE WRK-AREA-ERRO-CICS
                                       TO WRK-BLOCO-INFO-ERRO
                                         (1:FRWKGHEA-TAM-DADOS)

               WHEN ERRO-MODULO
                   MOVE FRWKGMOD-TAM-LAYOUT
                                       TO FRWKGHEA-TAM-DADOS
                   MOVE WRK-AREA-ERRO-MODULO
                                       TO WRK-BLOCO-INFO-ERRO
                                         (1:FRWKGHEA-TAM-DADOS)
           END-EVALUATE.

           PERFORM 9999-API-ERROS.

      *----------------------------------------------------------------*
       9998-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       9999-API-ERROS                  SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
                LINK
                PROGRAM  (WRK-FRWK1999)
                COMMAREA (WRK-AREA-ERRO)
                LENGTH   (LENGTH OF WRK-AREA-ERRO)
                NOHANDLE
           END-EXEC.

           IF  EIBRESP            NOT EQUAL  DFHRESP(NORMAL)
               CONTINUE
           END-IF.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*