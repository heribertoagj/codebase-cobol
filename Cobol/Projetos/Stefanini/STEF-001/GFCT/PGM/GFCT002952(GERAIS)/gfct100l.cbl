      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT100L.
       AUTHOR.     SONDAPROCWORK.
      *================================================================*
      *----------------------------------------------------------------*
      *    PROGRAMA....: GFCT100L                                      *
      *    PROGRAMADOR.: FABRICA SONDA PROCWORK                        *
      *    ANALISTA....: FABRICA SONDA PROCWORK    - SONDA PROCWORK    *
      *    ANALISTA....: FABRICA SONDA PROCWORK    - TI MELHORIAS      *
      *    DATA........: JUN/2013                                      *
      *----------------------------------------------------------------*
      *    OBJETIVO....: CONSULTAR LISTA DE CLIENTES SEM CESTA         *
      *----------------------------------------------------------------*
      *    BOOKS FUNCIONAIS:                                           *
      *    GFCTW10E - AREA DE INTERFACE COM FRWK PARA DADOS DE ENTRADA.*
      *    GFCTW10S - AREA DE INTERFACE COM FRWK PARA DADOS DE SAIDA.  *
      *    GFCTW10W - AREA DE GRAVACAO DE DADOS PERSISTENTES.          *
      *    GFCTW100 - BOOK DE CONTROLE DO ACESSO AO SERVICO FUNCIONAL  *
      *    GFCTW10I - BOOK DE ACESSO AO SERVICO FUNCIONAL              *
      *                                                                *
      *    BOOKS ARQUITETURAIS:                                        *
      *    I#FRWKAQ COMMAREA ENTRE PROGRAMA/FRAMEWORK                  *
      *    I#FRWK04 COMMAREA ENTRE PROGRAMA/FRWK1200(GAM)              *
      *    FRWKWAAA COMMAREA ENTRE PROGRAMA/FRWK1200 - DADOS SESSAO    *
      *    I#FRWKGE COMMAREA FRWK1999 (LOG DE ERRO)                    *
      *    I#FRWKLI COMMAREA FRWK1999 (LOG DE ERROS LIVRES)            *
      *    I#FRWKMD COMMAREA FRWK1999 (LOG DE ERROS MODULO)            *
      *    I#FRWKCI COMMAREA FRWK1999 (LOG DE ERROS CICS)              *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    FRWK1999 - PROCEDIMENTOS PARA GRAVACAO DE LOGS DE ERRO.     *
      *    FRWK1200 - MODULO DE ACESSO AO GAM.                         *
      *    GFCT300L - MODULO FUNCIONAL ACESSADO                        *
      *================================================================*
      *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.
      *
      *================================================================*
       DATA                            DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC X(050)  VALUE
           '*** INICIO DA WORKING GFCT100L ***'.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC X(050)  VALUE
           'AREA PARA VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           05 WRK-FRWK1999             PIC X(008)  VALUE 'FRWK1999'.
           05 WRK-FRWK1200             PIC X(008)  VALUE 'FRWK1200'.
           05 WRK-PROGRAMA             PIC X(008)  VALUE 'GFCT100L'.
           05 WRK-GFCT300L             PIC X(08)   VALUE 'GFCT300L'.
           05 WRK-TRATAMENTO-ERROS.
             10 WRK-BLOCO-RETORNO.
               15 WRK-COD-RETORNO      PIC 9(002)  VALUE ZEROS.
               15 WRK-COD-ERRO         PIC X(004)  VALUE SPACES.
               15 WRK-COD-MENSAGEM     PIC X(008)  VALUE SPACES.
             10 WRK-MENSAGEM           PIC X(008)  VALUE SPACES.
             10 WRK-IDIOMA             PIC 9(005)  VALUE ZEROS.
             10 WRK-PARAMETROS         PIC X(500)  VALUE SPACES.
      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC X(050)  VALUE
           'AREA PARA INDEXADORES'.
      *----------------------------------------------------------------*
       77  IND-1                       PIC 9(003)  COMP-3 VALUE ZEROS.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)  VALUE
           'AREA DE COMUNICACAO DO FRWK1999 (LOG DE ERRO)'.
      *----------------------------------------------------------------*
       01  WRK-AREA-ERRO.
           COPY  'I#FRWKGE'.
           05 WRK-BLOCO-INFO-ERRO.
             10 WRK-CHAR-INFO-ERRO     PIC X(01) OCCURS 0 TO 30000 TIMES
                                       DEPENDING ON FRWKGHEA-TAM-DADOS.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)  VALUE
           'AREA DE TRATAMENTO DE ERRO LIVRE (API-ERRO)'.
      *----------------------------------------------------------------*
       01  WRK-COPY-LIVRE.
           COPY  'I#FRWKLI'.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)  VALUE
           'AREA DE TRATAMENTO DE ERRO MODULO (API-ERRO)'.
      *----------------------------------------------------------------*
       01  WRK-COPY-MOD.
           COPY  'I#FRWKMD'.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)  VALUE
           'AREA DE TRATAMENTO DE ERRO CICS(API-ERRO)'.
      *----------------------------------------------------------------*
       01  WRK-COPY-CICS.
           COPY  'I#FRWKCI'.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)  VALUE
           'AREA DE TRATAMENTO DE INTERFACE (GAM)'.
      *----------------------------------------------------------------*
       01  WRK-AREA-FRWK1200.
           COPY  'I#FRWK04'.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)  VALUE
           'AREA PARA BUSCAR DADOS DE SESSAO'.
      *----------------------------------------------------------------*
       01  WRK-AREA-FRWKAAA.
           COPY FRWKWAAA.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)  VALUE
           'AREA DE INTERFACE PARA DADOS DE ENTRADA'.
      *----------------------------------------------------------------*
       01  WRK-GFCTW10E.
           COPY GFCTW10E.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)  VALUE
           'AREA DE INTERFACE PARA DADOS DE PERSISTENCIA'.
      *----------------------------------------------------------------*
       01  WRK-GFCTW10W.
           COPY GFCTW10W.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)  VALUE
      *
           'AREA DE INTERFACE PARA DADOS DE SAIDA'.
      *----------------------------------------------------------------*
       01  WRK-GFCTW10S.
           COPY GFCTW10S.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)  VALUE
           'AREA DE INTERFACE COM SERVICO FUNCIONAL'.
      *----------------------------------------------------------------*
       01  WRK-AREA-GFCT300L.
           COPY GFCTW100.
           COPY GFCTW10I.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)  VALUE
           '*** FIM DA WORKING GFCT100L ***'.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       01  DFHCOMMAREA.
           COPY  'I#FRWKAQ'.
      *
      *================================================================*
       PROCEDURE                       DIVISION  USING DFHCOMMAREA.
      *================================================================*
      *
      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL DO PROGRAMA.                               *
      *----------------------------------------------------------------*
       0000-PRINCIPAL                  SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 1000-INICIALIZAR
      *
           PERFORM 2000-PROCESSAR
      *
           PERFORM 3000-FINALIZAR.
      *
      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA INICIAL DO PROGRAMA.                                 *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE FRWKGHEA-REGISTRO
                      FRWKGCIC-REGISTRO
                      FRWKGMOD-REGISTRO
      *
           PERFORM 6000-OBTER-DADOS-ENTRADA
      *
           PERFORM 6100-RECUPERA-DADOS-SESSAO
      *
           PERFORM 1100-CONSISTIR-DADOS-ENTRADA.
      *
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA CONSISTENCIA DOS DADOS DE ENTRADA.              *
      *----------------------------------------------------------------*
       1100-CONSISTIR-DADOS-ENTRADA    SECTION.
      *----------------------------------------------------------------*
      *

           IF (GFCTW10E-CFUNC-BDSCO       NOT NUMERIC)
               MOVE 08                 TO FRWKGLAQ-COD-RETORNO
               MOVE '0010'             TO FRWKGLAQ-COD-ERRO
               MOVE 'GFCT0002'         TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF

           IF (GFCTW10E-CAG-BCRIA         NOT NUMERIC  OR
               GFCTW10E-CAG-BCRIA         EQUAL ZEROS)
               MOVE 08                 TO FRWKGLAQ-COD-RETORNO
               MOVE '0011'             TO FRWKGLAQ-COD-ERRO
               MOVE 'GFCT0002'         TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF

           .
      *
      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA.                        *
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*
      *
           EVALUATE TRUE
             WHEN FRWKWAAA-CONSULTA-INICIAL
             WHEN FRWKWAAA-PRIMEIRO-BLOCO
             WHEN FRWKWAAA-ULTIMO-BLOCO
                  PERFORM 2100-CONSULTA-INICIAL
             WHEN FRWKWAAA-BLOCO-SEGUINTE
             WHEN FRWKWAAA-BLOCO-ANTERIOR
                  PERFORM 2200-PAGINAR
             WHEN OTHER
                  MOVE '0030'          TO FRWKGLAQ-COD-ERRO
                  MOVE 'GFCT0002'      TO FRWKGLAQ-COD-MENSAGEM
                  MOVE '2000-PROCESSAR'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
                  MOVE FRWKGLAQ-COD-MENSAGEM
                                       TO WRK-MENSAGEM
                  MOVE FRWKWAAA-WINFO-DADOS-IDIOM
                                       TO WRK-IDIOMA
                  MOVE SPACES          TO WRK-PARAMETROS
                  PERFORM 9200-ERRO-LIVRE
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA PRIMEIRA CONSULTA DO PROGRAMA                   *
      *----------------------------------------------------------------*
       2100-CONSULTA-INICIAL           SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 2210-FORMATA-COMMAREA
      *
           PERFORM 2300-ACESSAR-FUNCIONAL
      *
           PERFORM 2400-MOVER-DADOS-SAIDA
      *
           PERFORM 6200-INFORMAR-BLOCO-SAIDA
      *
           IF (GFCTW100-COD-RETORNO    OF WRK-AREA-GFCT300L
                                       EQUAL 01)
                IF (FRWKWAAA-CONSULTA-INICIAL)
                  PERFORM 6300-CRIAR-GAM-PERSISTENTE
               ELSE
                  PERFORM 6400-ATUALIZAR-GAM-PERSISTENTE
               END-IF
           END-IF.
      *
      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA PAGINAR A CONSULTA.                             *
      *----------------------------------------------------------------*
       2200-PAGINAR                    SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 6600-CONSULTAR-GAM-PERSISTENTE
      *
           PERFORM 6700-CONSISTIR-CHAVES
      *
           PERFORM 2210-FORMATA-COMMAREA
      *
           PERFORM 2220-FORMATA-CHAVES
      *
           PERFORM 2300-ACESSAR-FUNCIONAL
      *
           PERFORM 2400-MOVER-DADOS-SAIDA
      *
           PERFORM 6200-INFORMAR-BLOCO-SAIDA
      *
           PERFORM 6400-ATUALIZAR-GAM-PERSISTENTE.
      *
      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA FORMATAR O COPYBOOK DO SERVICO FUNCIONAL        *
      *----------------------------------------------------------------*
       2210-FORMATA-COMMAREA           SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE                  GFCTW100-BLOCO-RETORNO
                                       OF WRK-AREA-GFCT300L
                                       GFCTW10I-ENTRADA
                                       GFCTW10I-BLOCO-PAGINACAO
                                       REPLACING ALPHANUMERIC BY SPACES
                                                      NUMERIC BY ZEROS
           MOVE FRWKWAAA-SOLIC-MAIS-DADOS
                                       TO GFCTW10I-INDICADOR-PAGINACAO
           MOVE 50                     TO GFCTW10I-S-QTD-LISTA-OCORR
           MOVE GFCTW10E-CFUNC-BDSCO   TO GFCTW10I-E-CFUNC-BDSCO
           MOVE GFCTW10E-CAG-BCRIA     TO GFCTW10I-E-CAG-BCRIA
           .
      *
      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA FORMATAR OS CAMPOS CHAVES PARA VALIDACAO        *
      *----------------------------------------------------------------*
       2220-FORMATA-CHAVES             SECTION.
      *----------------------------------------------------------------*
      *
           MOVE GFCTW10W-CHAVE-INI     TO GFCTW10I-CHAVE-INI
           MOVE GFCTW10W-CHAVE-FIM     TO GFCTW10I-CHAVE-FIM.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA ACESSO AO MODULO FUNCIONAL.                     *
      *----------------------------------------------------------------*
       2300-ACESSAR-FUNCIONAL          SECTION.
      *----------------------------------------------------------------*
      *
           EXEC CICS LINK
                PROGRAM  (WRK-GFCT300L)
                COMMAREA (WRK-AREA-GFCT300L)
                LENGTH   (LENGTH OF WRK-AREA-GFCT300L)
                NOHANDLE
           END-EXEC
      *
           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE '0040'             TO FRWKGLAQ-COD-ERRO
               MOVE '2300-ACESSAR-FUNCIONAL'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9400-ERRO-CICS
           END-IF
      *
           EVALUATE GFCTW100-COD-RETORNO
             WHEN 00
             WHEN 01
               MOVE GFCTW100-BLOCO-RETORNO
                                       TO FRWKGLAQ-BLOCO-RETORNO
             WHEN 08
               MOVE GFCTW100-BLOCO-RETORNO
                                       TO FRWKGLAQ-BLOCO-RETORNO
               IF GFCTW100-COD-MENSAGEM EQUAL 'GFCT0012'
                  MOVE 00              TO FRWKGLAQ-COD-RETORNO
               END-IF
               PERFORM 3000-FINALIZAR
      *
             WHEN OTHER
               MOVE '0050'             TO FRWKGLAQ-COD-ERRO
               MOVE GFCTW100-COD-MENSAGEM
                                       TO FRWKGLAQ-COD-MENSAGEM
               MOVE WRK-GFCT300L       TO FRWKGMOD-NOME-MODULO
               MOVE GFCTW100-BLOCO-RETORNO
                                       TO WRK-BLOCO-RETORNO
               MOVE '2300-ACESSAR-FUNCIONAL'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9300-ERRO-MODULO
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA RESPONSAVEL POR FORMATAR A AREA DE SAIDA.            *
      *----------------------------------------------------------------*
       2400-MOVER-DADOS-SAIDA          SECTION.
      *----------------------------------------------------------------*
      *
           MOVE GFCTW10I-S-QTD-LISTA-OCORR
                                       TO GFCTW10S-QTD-LISTA-OCORR

           PERFORM VARYING IND-1 FROM 1 BY 1
                   UNTIL   IND-1 GREATER GFCTW10S-QTD-LISTA-OCORR
           MOVE GFCTW10I-S-IPSSOA-COPLT(IND-1)
                                   TO GFCTW10S-S-IPSSOA-COPLT(IND-1)
           MOVE GFCTW10I-S-CSERVC-TARIF(IND-1)
                                   TO GFCTW10S-S-CSERVC-TARIF(IND-1)
           MOVE GFCTW10I-S-CINDCD-VINCLO-FL(IND-1)
                                   TO GFCTW10S-S-CINDCD-VINCLO-FL(IND-1)
           MOVE GFCTW10I-S-CCTA-BCRIA(IND-1)
                                   TO GFCTW10S-S-CCTA-BCRIA(IND-1)
           MOVE GFCTW10I-S-DABERT-CTA(IND-1)
                                   TO GFCTW10S-S-DABERT-CTA(IND-1)
           MOVE GFCTW10I-S-VRENTB(IND-1)
                                   TO GFCTW10S-S-VRENTB(IND-1)
           MOVE GFCTW10I-S-SINAL-VRENTB(IND-1)
                                   TO GFCTW10S-S-SINAL-VRENTB(IND-1)
           MOVE GFCTW10I-S-VINVES(IND-1)
                                   TO GFCTW10S-S-VINVES(IND-1)
           MOVE GFCTW10I-S-SINAL-VINVES(IND-1)
                                   TO GFCTW10S-S-SINAL-VINVES(IND-1)
           MOVE GFCTW10I-S-CDDI(IND-1)
                                   TO GFCTW10S-S-CDDI(IND-1)
           MOVE GFCTW10I-S-CDDD(IND-1)
                                   TO GFCTW10S-S-CDDD(IND-1)
           MOVE GFCTW10I-S-CID-RMAL-BIP-PAGER(IND-1)
                                   TO
                                   GFCTW10S-S-CID-RMAL-BIP-PAGER(IND-1)
           MOVE GFCTW10I-S-NLIN-TFONI(IND-1)
                                   TO GFCTW10S-S-NLIN-TFONI(IND-1)
           MOVE GFCTW10I-S-CINDCD-REST-CNTAT(IND-1)
                                   TO
                                   GFCTW10S-S-CINDCD-REST-CNTAT(IND-1)
           MOVE GFCTW10I-S-CCPF-CNPJ(IND-1)
                                   TO GFCTW10S-S-CCPF-CNPJ(IND-1)
           MOVE GFCTW10I-S-CCTRL-CPF-CNPJ(IND-1)
                                   TO GFCTW10S-S-CCTRL-CPF-CNPJ(IND-1)
           MOVE GFCTW10I-S-CPSSOA(IND-1)
                                   TO GFCTW10S-S-CPSSOA(IND-1)
           MOVE GFCTW10I-S-CFUNC-BDSCO(IND-1)
                                   TO GFCTW10S-S-CFUNC-BDSCO(IND-1)
           MOVE GFCTW10I-E-CAG-BCRIA
                                   TO GFCTW10S-S-CAG-BCRIA(IND-1)
           END-PERFORM.


      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZAR O PROCESSAMENTO.                      *
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*
      *
           EXEC CICS
               RETURN
           END-EXEC.
      *
      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    EFETUAR CHAMADA AO MODULO FRWK1200 - GAM                    *
      *----------------------------------------------------------------*
       5000-ACESSAR-GAM                SECTION.
      *----------------------------------------------------------------*
      *
           EXEC CICS LINK
                PROGRAM  (WRK-FRWK1200)
                COMMAREA (WRK-AREA-FRWK1200)
                LENGTH   (LENGTH OF WRK-AREA-FRWK1200)
                NOHANDLE
           END-EXEC
      *
           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE '0060'             TO FRWKGLAQ-COD-ERRO
               MOVE '5000-ACESSAR-GAM' TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9400-ERRO-CICS
           END-IF
      *
           IF (FRWKEL04-COD-RETORNO    NOT EQUAL ZEROS)
               MOVE '0070'             TO FRWKGLAQ-COD-ERRO
               MOVE FRWKEL04-COD-MENSAGEM
                                       TO FRWKGLAQ-COD-MENSAGEM
               MOVE WRK-FRWK1200       TO FRWKGMOD-NOME-MODULO
               MOVE FRWKEL04-BLOCO-RETORNO
                                       TO WRK-BLOCO-RETORNO
               MOVE '5000-ACESSAR-GAM' TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9300-ERRO-MODULO
           END-IF.
      *
      *----------------------------------------------------------------*
       5000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA OBTER OS DADOS DE ENTRADA.                      *
      *----------------------------------------------------------------*
       6000-OBTER-DADOS-ENTRADA        SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE FRWKEL04-BLOCO-ENTRADA
                      FRWKEL04-BLOCO-RETORNO
      *
           SET  FRWKEL04-FUN-CONSULTAR TO TRUE
           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO FRWKEL04-IDENT-AREA-GLOBAL
           MOVE GFCTW10E-COD-LAYOUT    TO FRWKEL04-ID-BLOCO
           MOVE GFCTW10E-TAM-LAYOUT    TO FRWKEL04-TAM-BLOCO
      *
           PERFORM 5000-ACESSAR-GAM
      *
           MOVE FRWKEL04-BUFFER(1:FRWKEL04-TAM-BLOCO)
                                       TO WRK-GFCTW10E.
      *
      *----------------------------------------------------------------*
       6000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA RECUPERAR DADOS DE SESSAO.                      *
      *----------------------------------------------------------------*
       6100-RECUPERA-DADOS-SESSAO      SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE FRWKEL04-BLOCO-ENTRADA
                      FRWKEL04-BLOCO-RETORNO
      *
           SET FRWKEL04-FUN-CONSULTAR  TO TRUE
           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO FRWKEL04-IDENT-AREA-GLOBAL
           MOVE 'FRWKEL83'             TO FRWKEL04-ID-BLOCO
           MOVE FRWKWAAA-TAM-LAYOUT    TO FRWKEL04-TAM-BLOCO
      *
           PERFORM 5000-ACESSAR-GAM
      *
           MOVE FRWKEL04-BUFFER(1:FRWKEL04-TAM-BLOCO)
                                       TO WRK-AREA-FRWKAAA.
      *
      *----------------------------------------------------------------*
       6100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA FORMATAR OS DADOS DE SAUDA                      *
      *----------------------------------------------------------------*
       6200-INFORMAR-BLOCO-SAIDA       SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE FRWKEL04-BLOCO-ENTRADA
                      FRWKEL04-BLOCO-RETORNO
      *
           SET FRWKEL04-FUN-CRIAR      TO TRUE
           SET FRWKEL04-PERM-CONSULTAR TO TRUE
           SET FRWKEL04-IND-BL-SAIDA   TO TRUE
           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO FRWKEL04-IDENT-AREA-GLOBAL
           MOVE GFCTW10S-COD-LAYOUT    TO FRWKEL04-ID-BLOCO
      *
           MOVE LENGTH OF WRK-GFCTW10S
                                       TO FRWKEL04-TAM-BLOCO
                                       GFCTW10S-TAM-LAYOUT
           MOVE WRK-GFCTW10S           TO FRWKEL04-BUFFER
                                                  (1:FRWKEL04-TAM-BLOCO)
      *
           PERFORM 5000-ACESSAR-GAM.
      *
      *----------------------------------------------------------------*
       6200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA CRIACAO DO BLOCO DE PERSISTENCIA.               *
      *----------------------------------------------------------------*
       6300-CRIAR-GAM-PERSISTENTE      SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE FRWKEL04-BLOCO-ENTRADA
                      FRWKEL04-BLOCO-RETORNO
      *
           SET FRWKEL04-FUN-CRIAR      TO TRUE
           SET FRWKEL04-PERM-RESTRITO  TO TRUE
           SET FRWKEL04-IND-BL-PERS    TO TRUE
      *
           PERFORM 6500-EFETIVA-PERSISTENCIA-GAM.
      *
      *----------------------------------------------------------------*
       6300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA ATUALIZACAO DO BLOCO DE PERSISTENCIA.           *
      *----------------------------------------------------------------*
       6400-ATUALIZAR-GAM-PERSISTENTE  SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE FRWKEL04-BLOCO-ENTRADA
                      FRWKEL04-BLOCO-RETORNO
      *
           SET FRWKEL04-FUN-ATUALIZAR  TO TRUE
           SET FRWKEL04-PERM-ATUALIZAR TO TRUE
           SET FRWKEL04-IND-BL-PERS    TO TRUE
      *
           PERFORM 6500-EFETIVA-PERSISTENCIA-GAM.
      *
      *----------------------------------------------------------------*
       6400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA EFETIVAR A PERSISTENCIA NO GAM                  *
      *----------------------------------------------------------------*
       6500-EFETIVA-PERSISTENCIA-GAM   SECTION.
      *----------------------------------------------------------------*
           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO FRWKEL04-IDENT-AREA-GLOBAL
      *
           MOVE GFCTW10W-COD-LAYOUT    TO FRWKEL04-ID-BLOCO
           MOVE GFCTW10W-TAM-LAYOUT    TO FRWKEL04-TAM-BLOCO
      *
           MOVE FRWKWAAA-SOLIC-MAIS-DADOS
                                       TO GFCTW10W-INDICADOR-PAGINACAO
      *
           MOVE GFCTW10I-CHAVE-INI     TO GFCTW10W-CHAVE-INI
           MOVE GFCTW10I-CHAVE-FIM     TO GFCTW10W-CHAVE-FIM

      *
           MOVE GFCTW10E-ENTRADA       TO GFCTW10W-ENTRADA
      *
           MOVE GFCTW100-COD-RETORNO   TO GFCTW10W-COD-RETORNO
      *
           MOVE WRK-GFCTW10W           TO FRWKEL04-BUFFER
                                         (1:FRWKEL04-TAM-BLOCO)
      *
           PERFORM 5000-ACESSAR-GAM.
      *
      *----------------------------------------------------------------*
       6500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA CONSULTAR O BLOCO DE PERSISTENCIA.              *
      *----------------------------------------------------------------*
       6600-CONSULTAR-GAM-PERSISTENTE  SECTION.
      *----------------------------------------------------------------*
      *
           SET  FRWKEL04-FUN-CONSULTAR TO TRUE
      *
           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO FRWKEL04-IDENT-AREA-GLOBAL
           MOVE GFCTW10W-COD-LAYOUT    TO FRWKEL04-ID-BLOCO
           MOVE GFCTW10W-TAM-LAYOUT    TO FRWKEL04-TAM-BLOCO
      *
           PERFORM 5000-ACESSAR-GAM
      *
           MOVE FRWKEL04-BUFFER(1:FRWKEL04-TAM-BLOCO)
                                       TO WRK-GFCTW10W.
      *
      *----------------------------------------------------------------*
       6600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     ROTINA PARA CONSISTENCIA DE CHAVES.                        *
      *----------------------------------------------------------------*
       6700-CONSISTIR-CHAVES           SECTION.
      *----------------------------------------------------------------*
      *
           IF ((GFCTW10W-P-INICIAL     OR GFCTW10W-P-PRIMEIRA)
           AND  FRWKWAAA-BLOCO-ANTERIOR)
           OR  (GFCTW10W-P-ULTIMA      AND FRWKWAAA-BLOCO-SEGUINTE)
           OR  (GFCTW10W-P-SEGUINTE    AND FRWKWAAA-BLOCO-SEGUINTE
           AND  GFCTW10W-COD-RETORNO   EQUAL ZEROS)
           OR  (GFCTW10W-P-ANTERIOR    AND FRWKWAAA-BLOCO-ANTERIOR
           AND  GFCTW10W-COD-RETORNO   EQUAL ZEROS)
               MOVE '0080'             TO FRWKGLAQ-COD-ERRO
               MOVE 'GFCT0011'         TO FRWKGLAQ-COD-MENSAGEM
               MOVE '6700-CONSISTIR-CHAVES'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE FRWKGLAQ-COD-MENSAGEM
                                       TO WRK-MENSAGEM
               MOVE FRWKWAAA-WINFO-DADOS-IDIOM
                                       TO WRK-IDIOMA
               MOVE SPACES             TO WRK-PARAMETROS
               PERFORM 9200-ERRO-LIVRE
           END-IF
      *
           IF (GFCTW10E-CFUNC-BDSCO NOT EQUAL GFCTW10W-E-CFUNC-BDSCO)
               AND (GFCTW10E-CAG-BCRIA NOT EQUAL GFCTW10W-E-CAG-BCRIA)

               MOVE '0090'             TO FRWKGLAQ-COD-ERRO
               MOVE 'GFCT0002'         TO FRWKGLAQ-COD-MENSAGEM
               MOVE '6700-CONSISTIR-CHAVES'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE FRWKGLAQ-COD-MENSAGEM
                                       TO WRK-MENSAGEM
               MOVE FRWKWAAA-WINFO-DADOS-IDIOM
                                       TO WRK-IDIOMA
               MOVE SPACES             TO WRK-PARAMETROS
               PERFORM 9200-ERRO-LIVRE
           END-IF.
      *
      *----------------------------------------------------------------*
       6700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO LIVRE.                       *
      *----------------------------------------------------------------*
       9200-ERRO-LIVRE                 SECTION.
      *----------------------------------------------------------------*
      *
           SET  ERRO-LIVRE             TO TRUE
           MOVE 16                     TO FRWKGLAQ-COD-RETORNO
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA
           MOVE FRWKGLIV-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS
           MOVE WRK-MENSAGEM           TO FRWKGLIV-COD-MENSAGEM
           MOVE WRK-IDIOMA             TO FRWKGLIV-IDIOMA
           MOVE WRK-PARAMETROS         TO FRWKGLIV-PARAMETROS
           MOVE WRK-COPY-LIVRE         TO WRK-BLOCO-INFO-ERRO
                                          (1:30000)
           PERFORM 9999-API-ERROS.
      *
      *----------------------------------------------------------------*
       9200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO MODULO                       *
      *----------------------------------------------------------------*
       9300-ERRO-MODULO                SECTION.
      *----------------------------------------------------------------*
      *
           SET  ERRO-MODULO            TO TRUE
           MOVE 12                     TO FRWKGLAQ-COD-RETORNO
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA
           MOVE FRWKGMOD-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS
           MOVE WRK-BLOCO-RETORNO      TO FRWKGMOD-BLOCO-RETORNO
           MOVE WRK-COPY-MOD           TO WRK-BLOCO-INFO-ERRO
      *
           PERFORM 9999-API-ERROS.
      *
      *----------------------------------------------------------------*
       9300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO CICS.                        *
      *----------------------------------------------------------------*
       9400-ERRO-CICS                  SECTION.
      *----------------------------------------------------------------*
      *
           SET  ERRO-CICS              TO TRUE
           MOVE 16                     TO FRWKGLAQ-COD-RETORNO
           MOVE 'GFCT0006'             TO FRWKGLAQ-COD-MENSAGEM
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA
           MOVE FRWKGCIC-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS
           MOVE EIBFN                  TO FRWKGCIC-EIBFN
           MOVE EIBRCODE               TO FRWKGCIC-EIBRCODE
           MOVE EIBRSRCE               TO FRWKGCIC-EIBRSRCE
           MOVE EIBRESP                TO FRWKGCIC-EIBRESP
           MOVE EIBRESP2               TO FRWKGCIC-EIBRESP2
           MOVE EIBTASKN               TO FRWKGCIC-EIBTASKN
           MOVE WRK-COPY-CICS          TO WRK-BLOCO-INFO-ERRO
      *
           PERFORM 9999-API-ERROS.
      *
      *----------------------------------------------------------------*
       9400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO CHAMADA AO FRWK1999.                 *
      *----------------------------------------------------------------*
       9999-API-ERROS                  SECTION.
      *----------------------------------------------------------------*
      *
           EXEC CICS LINK
                PROGRAM  (WRK-FRWK1999)
                COMMAREA (WRK-AREA-ERRO)
                LENGTH   (LENGTH OF WRK-AREA-ERRO)
                NOHANDLE
           END-EXEC
      *
           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF
      *
           PERFORM 3000-FINALIZAR.
      *
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
