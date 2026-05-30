      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. CTEC3401.
       AUTHOR.     HERIBERTO GIANNASI.
      *================================================================*
      *                      S T E F A N I N I                         *
      *================================================================*
      *    PROGRAMA....: CTEC3401                                      *
      *    ANALISTA....:  HERIBERTO GIANNASI               - STEFANINI *
      *    DATA........:  10/2024                                      *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  PROGRAMA FUNCIONAL QUE ATENDE A FUNCAO       *
      *                   DE OBTER NOME DE FUNCIONARIO                 *  
      *----------------------------------------------------------------*
      *    BOOKS FUNCIONAIS:                                           *
      *    CTECW00I - BOOK DE CONTROLE / RETORNO                       *
      *    CTECWDAI - BOOK DE COMUNICACAO ENTRE COORDENADOR E FUNCIONAL*
      *----------------------------------------------------------------*
      *    BOOKS ARQUITETURAIS:                                        *
      *    I#FRWKGE COMMAREA FRWK1999 (LOG DE ERRO)                    *
      *    I#FRWKHE COMMAREA FRWK1999 (AREA COMUM ERROS)               *
      *    I#FRWKD2 COMMAREA FRWK1999 (LOG DE ERROS DB2)               *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    FRWK1999 - PROCEDIMENTOS PARA GRAVACAO DE LOGS DE ERRO      *
      *================================================================*
      *                                                                *
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
       77  FILLER                      PIC  X(050) VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050) VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           05 WRK-FRWK1999             PIC  X(008) VALUE 'FRWK1999'.
           05 WRK-PROGRAMA             PIC  X(008) VALUE 'CTEC3AIL'.
           05 WRK-MODULO               PIC  X(008) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(051)      VALUE
           '*   AREA PARA COMUNICACAO COM O GSEG1508   *'.
      *----------------------------------------------------------------*
       01  WRK-AREA-GSEG1508.
           COPY GSEGW508.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)     VALUE
           'AREA PARA O MODULO SARH9000'.
      *----------------------------------------------------------------*
       01  WRK-AREA-SARH9000.
           COPY CTECWSAI.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)      VALUE
           '* AREA DE COMUNICACAO DO PROGRAMA FRWK1999 *'.
      *----------------------------------------------------------------*
       01  WRK-AREA-ERRO.
           COPY 'I#FRWKGE'.
           05 WRK-BLOCO-INFO-ERRO.
              10 WRK-CHAR-INFO-ERRO    PIC X(001) OCCURS 0
                                       TO 526 TIMES
                                       DEPENDING ON FRWKGHEA-TAM-DADOS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)      VALUE
           'AREA DE TRATAMENTO DE ERRO CICS(API-ERRO)'.
      *----------------------------------------------------------------*
       01  WRK-COPY-CICS.
           COPY 'I#FRWKCI'.
      
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)      VALUE
           'AREA DE TRATAMENTO DE ERRO MODULO (API-ERRO)'.
      *----------------------------------------------------------------*
       01  WRK-COPY-MOD.
           COPY 'I#FRWKMD'.
      
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)      VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       01  DFHCOMMAREA.
           COPY CTECW00I.
           COPY CTECWDCI.

      *================================================================*
       PROCEDURE                       DIVISION USING DFHCOMMAREA.
      *================================================================*
      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL DO PROGRAMA                                *
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
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           IF  CTECWDCI-E-CUSUAR       NOT NUMERIC
           OR  CTECWDCI-E-CUSUAR       EQUAL ZEROS
               MOVE 08                 TO CTECW00I-COD-RETORNO
               MOVE '0010'             TO CTECW00I-COD-ERRO
               MOVE 'CTEC0046'         TO CTECW00I-COD-MENSAGEM
               PERFORM  3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.                  
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*
            
           PERFORM 2100-BUSCAR-NM-FUNC.

           PERFORM 2200-MOVER-SAIDA.

           MOVE ZEROS                 TO CTECW00I-COD-RETORNO.
           MOVE '0000'                TO CTECW00I-COD-ERRO.
           
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT. 
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2100-BUSCAR-NM-FUNC             SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE                  CTECWSAI-BLOCO-ENTRADA
                                       OF WRK-AREA-SARH9000
                                       REPLACING ALPHANUMERIC BY SPACES
                                                       NUMERIC BY ZEROS
      
           MOVE 'CTEC'                 TO CTECWSAI-E-SISTEMA
      
           PERFORM 2110-CONVERTER-USUARIO.
      
           MOVE GSEGW508-FUNCIONAL-SAI TO CTECWSAI-E-PESQUISA.
           MOVE 86                     TO CTECWSAI-E-TAMANHO.
           MOVE 2                      TO CTECWSAI-E-CAMPO(1).
      
           MOVE 'SARH9000'             TO WRK-MODULO.
      
           EXEC CICS LINK
                PROGRAM (WRK-MODULO)
                COMMAREA (WRK-AREA-SARH9000)
                LENGTH (LENGTH OF WRK-AREA-SARH9000)
                NOHANDLE
           END-EXEC.
      
           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE 16                 TO CTECW00I-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0280'             TO CTECW00I-COD-ERRO
                                       OF DFHCOMMAREA
               MOVE 'CTEC0030'         TO CTECW00I-COD-MENSAGEM
                                       OF DFHCOMMAREA
               MOVE '2100-BUSCAR-NM-FUNC'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9400-ERRO-CICS
           END-IF.
      
           IF CTECWSAI-S-COD-RETORNO   NOT EQUAL ZEROS
               MOVE 'USUARIO NAO ENCONTRADO'
                                       TO CTECWSAI-S-REGISTRO
                                          (1:CTECWSAI-S-TAMANHO)
               MOVE 08                 TO CTECW00I-COD-RETORNO
               MOVE '2100'             TO CTECW00I-COD-ERRO
               MOVE 'CTEC0046'         TO CTECW00I-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.
      
      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      * TRANSFORMA CODIGO USUARIO DE ALFANUMERICO PARA NUMERICO
      *----------------------------------------------------------------*
       2110-CONVERTER-USUARIO          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE GSEGW508-BLOCO-RETORNO
                      GSEGW508-BLOCO-ENTRADA
                      GSEGW508-BLOCO-SAIDA
                                       REPLACING ALPHANUMERIC BY SPACES
                                       NUMERIC BY ZEROS

           MOVE 1                      TO GSEGW508-VERSAO
           MOVE 1                      TO GSEGW508-FUNCAO
           MOVE CTECWDCI-E-CUSUAR      TO GSEGW508-COD-USUARIO-ENT
           MOVE ZEROS                  TO GSEGW508-FUNCIONAL-ENT
      *
           MOVE 'GSEG1508'             TO WRK-MODULO
      *
           EXEC CICS LINK
                PROGRAM  (WRK-MODULO)
                COMMAREA (WRK-AREA-GSEG1508)
                LENGTH   (LENGTH OF WRK-AREA-GSEG1508)
                NOHANDLE
           END-EXEC.
      *
           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE 16                 TO CTECW00I-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0290'             TO CTECW00I-COD-ERRO
                                       OF DFHCOMMAREA
               MOVE 'CTEC0030'         TO CTECW00I-COD-MENSAGEM
                                       OF DFHCOMMAREA
               MOVE '2110-CONVERTER-USUARIO'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9400-ERRO-CICS
           END-IF.
      *
           EVALUATE GSEGW508-COD-RETORNO
               WHEN ZEROS
                   CONTINUE
               WHEN 08
                    MOVE GSEGW508-COD-RETORNO
                                       TO CTECW00I-COD-RETORNO
                    MOVE GSEGW508-COD-ERRO
                                       TO CTECW00I-COD-ERRO
                    MOVE GSEGW508-COD-MENSAGEM
                                       TO CTECW00I-COD-MENSAGEM
                    PERFORM 3000-FINALIZAR
               WHEN OTHER
                    MOVE 16            TO CTECW00I-COD-RETORNO
                    MOVE '0300'        TO CTECW00I-COD-ERRO
                    MOVE GSEGW508-COD-MENSAGEM
                                       TO CTECW00I-COD-MENSAGEM
                    MOVE '2110-CONVERTER-USUARIO'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
                    MOVE GSEGW508-COD-RETORNO
                                       TO FRWKGMOD-COD-RETORNO
                    MOVE GSEGW508-COD-ERRO
                                       TO FRWKGMOD-COD-ERRO
                    MOVE GSEGW508-COD-MENSAGEM
                                       TO FRWKGMOD-COD-MENSAGEM
                    PERFORM 9300-ERRO-MODULO
           END-EVALUATE.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2200-MOVER-SAIDA                SECTION.
      *----------------------------------------------------------------*
           MOVE CTECWSAI-S-REGISTRO(1:CTECWSAI-S-TAMANHO)
                                       TO CTECWDCI-S-IUSUAR.
      
      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.      
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
      *    ROTINA PARA TRATAMENTO DE ERRO MODULO                       *
      *----------------------------------------------------------------*
       9300-ERRO-MODULO                SECTION.
      *----------------------------------------------------------------*
      *
           SET  ERRO-MODULO            TO TRUE.
           MOVE 16                     TO CTECW00I-COD-RETORNO.
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA.
           MOVE FRWKGMOD-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS.
           MOVE WRK-MODULO             TO FRWKGMOD-NOME-MODULO.
           MOVE WRK-COPY-MOD           TO WRK-BLOCO-INFO-ERRO
                                          (1:FRWKGHEA-TAM-DADOS).
           PERFORM 9999-API-ERROS.
      *
      *----------------------------------------------------------------*
       9300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO CICS.                        *
      *----------------------------------------------------------------*
       9400-ERRO-CICS                  SECTION.
      *----------------------------------------------------------------*
      *
           SET  ERRO-CICS              TO TRUE.
           MOVE 16                     TO CTECW00I-COD-RETORNO.
           MOVE 'CTEC0030'             TO CTECW00I-COD-MENSAGEM.
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA.
           MOVE FRWKGCIC-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS.
           MOVE EIBFN                  TO FRWKGCIC-EIBFN.
           MOVE EIBRCODE               TO FRWKGCIC-EIBRCODE.
           MOVE EIBRSRCE               TO FRWKGCIC-EIBRSRCE.
           MOVE EIBRESP                TO FRWKGCIC-EIBRESP.
           MOVE EIBRESP2               TO FRWKGCIC-EIBRESP2.
           MOVE EIBTASKN               TO FRWKGCIC-EIBTASKN.
           MOVE WRK-COPY-CICS          TO WRK-BLOCO-INFO-ERRO
                                          (1:FRWKGHEA-TAM-DADOS).
           PERFORM 9999-API-ERROS.
      *
      *----------------------------------------------------------------*
       9400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *   ROTINA PARA CHAMADA DA API DE ERROS                          *
      *----------------------------------------------------------------*
       9999-API-ERROS                  SECTION.
      *----------------------------------------------------------------*
      *
           EXEC CICS LINK
               PROGRAM  (WRK-FRWK1999)
               COMMAREA (WRK-AREA-ERRO)
               LENGTH   (LENGTH        OF WRK-AREA-ERRO)
               NOHANDLE
           END-EXEC.
      *
           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF.
      *
           PERFORM 3000-FINALIZAR.
      *
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
