      *----------------------------------------------------------------*
      *  SISTEMA : BVVE - CARTAO DE CREDITO / VENDEDOR                 *
      *  ARQUIVO : ARQUIVO DE CONFIRMACOES DE  DOMICILIO BANCARIO      *
      *            RECEBIDO DA REDECARD COM ERROS DE CONSISTENCIA      *
      *            FISICA                                              *
      *  LRECL   : 110 (FB)                                            *
      *  NOME INC: I#BVVEH0                                            *
      *  DATA    : 07/08/2008                                          *
      *----------------------------------------------------------------*
       01  BVVEH0-REG.
           05  BVVEH0-HEADER.
               10  BVVEH0H-TIPO-REG-X.
                   15  BVVEH0H-TIPO-REG           PIC 9(001).
               10  BVVEH0H-BANCO-ENVIO-X.
                   15  BVVEH0H-BANCO-ENVIO        PIC 9(003).
               10  BVVEH0H-DATA-ENVIO-X.
                   15  BVVEH0H-DATA-ENVIO         PIC 9(008).
               10  BVVEH0H-HORA-ENVIO-X.
                   15  BVVEH0H-HORA-ENVIO         PIC 9(006).
               10  BVVEH0H-INDICATIVO-BASE        PIC X(001).
               10  FILLER                         PIC X(037).
               10  BVVEH0H-DATA-CTRL-X.
                   15  BVVEH0H-DATA-CTRL          PIC 9(008).
               10  BVVEH0H-VERSAO-ARQ-X.
                   15  BVVEH0H-VERSAO-ARQ         PIC 9(009).
               10  FILLER                         PIC X(007).
           05  BVVEH0-DETALHE  REDEFINES  BVVEH0-HEADER.
               10  BVVEH0D-TIPO-REG-X.
                   15  BVVEH0D-TIPO-REG           PIC 9(001).
               10  BVVEH0D-BANCO-ENVIO-X.
                   15  BVVEH0D-BANCO-ENVIO        PIC 9(003).
               10  BVVEH0D-PONTO-VENDA-X.
                   15  BVVEH0D-PONTO-VENDA        PIC 9(009).
               10  BVVEH0D-NRO-SEQUENCIAL-X.
                   15  BVVEH0D-NRO-SEQUENCIAL     PIC 9(005).
               10  BVVEH0D-CNPJ.
                   15  BVVEH0D-CNPJ-NRO           PIC 9(008).
                   15  BVVEH0D-CNPJ-FILIAL        PIC 9(004).
                   15  BVVEH0D-CNPJ-CTRL          PIC 9(002).
               10  BVVEH0D-CPF REDEFINES BVVEH0D-CNPJ.
                   15  FILLER                     PIC X(003).
                   15  BVVEH0D-CPF-NRO            PIC 9(009).
                   15  BVVEH0D-CPF-CTRL           PIC 9(002).
               10  BVVEH0D-TIPO-PESSOA            PIC X(001).
               10  BVVEH0D-AGENCIA-X.
                   15  BVVEH0D-AGENCIA            PIC 9(005).
               10  BVVEH0D-CONTA-NRO-X.
                   15  BVVEH0D-CONTA-NRO          PIC 9(014).
               10  BVVEH0D-CONTA-DIG-X.
                   15  BVVEH0D-CONTA-DIG          PIC 9(001).
               10  BVVEH0D-COD-RETORNO-X.
                   15  BVVEH0D-COD-RETORNO        PIC 9(002).
               10  BVVEH0D-TIPO-ALTERACAO-X.
                   15  BVVEH0D-TIPO-ALTERACAO     PIC 9(001).
               10  BVVEH0D-DTSOLIC-ALTERA-X.
                   15  BVVEH0D-DTSOLIC-ALTERA     PIC 9(008).
               10  BVVEH0D-DTRETOR-BANCO-X.
                   15  BVVEH0D-DTRETOR-BANCO      PIC 9(008).
               10  BVVEH0D-TIPO-PROCES-X.
                   15  BVVEH0D-TIPO-PROCES        PIC 9(001).
               10  FILLER                         PIC X(007).
           05  BVVEH0-TRAILER  REDEFINES  BVVEH0-HEADER.
               10  BVVEH0T-TIPO-REG-X.
                   15  BVVEH0T-TIPO-REG           PIC 9(001).
               10  BVVEH0T-BANCO-ENVIO-X.
                   15  BVVEH0T-BANCO-ENVIO        PIC 9(003).
               10  BVVEH0T-QTDE-REG-X.
                   15  BVVEH0T-QTDE-REG           PIC 9(005).
               10  FILLER                         PIC X(047).
               10  BVVEH0T-DATA-CTRL-X.
                   15  BVVEH0T-DATA-CTRL          PIC 9(008).
               10  FILLER                         PIC X(016).
           05  BVVEH0-TAB-ERRO                    PIC 9(001) OCCURS 19.
           05  BVVEH0-QTDE-ERROS                  PIC 9(003) COMP-3.
           05  FILLER                             PIC X(009).
      *----------------------------------------------------------------*
      * 001-001 TIPO DE REGISTRO = 0(ZERO) / HEADER
      * 002-004 CODIGO DO BANCO QUE ESTA RECEBENDO/ENVIANDO O ARQUIVO
      * 005-012 DATA DO ENVIO DO ARQUIVO (AAAAMMDD)
      * 013-018 HORARIO DO ENVIO DO ARQUIVO (HHMMSS)
      * 019-019 INDICATIVO BASE (L=LEGADO E=ELETRONICO)
      * 020-056 ESPACO VAGO/RESERVADO
      * 057-064 DATA DE ENVIO PARA CONTROLE DE CLASSIFICACAO
      * 065-073 VERSAO DO ARQUIVO
      * 074-080 RESERVA PARA USO FUTURO
      *----------------------------------------------------------------*
      * 001-001 TIPO DE REGISTRO = 1 (DETALHE)
      * 002-004 CODIGO DO BANCO QUE ESTA RECEBENDO/ENVIANDO O ARQUIVO
      * 005-013 NUMERO DO PONTO DE VENDA DO ESTABELECIMENTO
      * 014-018 NUMERO SEQUENCIAL DE CONTROLE
      * 019-032 NUMERO DO CNPJ/CPF DO ESTABELECIMENTO
      * 033-033 TIPO DE PESSOA (F=FISICA J=JURIDICA)
      * 034-038 AGENCIA DA CONTA CORRENTE DO ESTABELECIMENTO(SEM DIGITO)
      * 039-052 NUMERO DA CONTA CORRENTE DO ESTABELECIMENTO
      * 053-053 DIGITO DA CONTA CORRENTE DO ESTABELECIMENTO
      * 054-055 CODIGO DE RETORNO DO BANCO
      * 056-056 TIPO DE ALTERACAO(1=GE 2=WF 3=LEGADO)
      * 057-064 DATA DA SOLICITACAO DA ALTERACAO (AAAAMMDD)
      * 065-072 DATA DE RETORNO DO BANCO (AAAAMMDD)
      * 073-073 TIPO DE PROCESSO (1=CONFIRMACAO DE DOMICILIO)
      * 074-080 RESERVA PARA USO FUTURO
      *----------------------------------------------------------------*
      * 001-001 TIPO DE REGISTRO = 9 (TRAILER)
      * 002-004 CODIGO DO BANCO QUE ESTA RECEBENDO/ENVIANDO O ARQUIVO
      * 005-009 QUANTIDADE DE REGISTROS (INCLUSIVE HEADER E TRAILER)
      * 010-056 ESPACO VAGO/RESERVADO
      * 057-064 DATA DE ENVIO PARA CONTROLE DE CLASSIFICACAO
      * 065-080 RESERVA PARA USO FUTURO
      *----------------------------------------------------------------*
      * 081-099 IDENTIFICADORES DE ERROS POR CAMPO   0=OK  1=COM ERRO
      *         APESAR DE PREVISTAS, AS OCORRENCIAS 14 A 19 NAO SERAO
      *         ASSINALADAS, POR ENQUANTO.
      * 081-081 OCOR-01 = TIPO DE REGISTRO INVALIDO
      * 082-082 OCOR-02 = BANCO DE ENVIO DIFERENTE DE 237
      * 083-083 OCOR-03 = DATA DE ENVIO INVALIDA
      * 084-084 OCOR-04 = HORARIO DE ENVIO INVALIDO
      * 085-085 OCOR-05 = DATA DE CONTROLE INVALIDA
      * 086-086 OCOR-06 = VERSAO DE ARQUIVO NAO NUMERICA OU ZERADA
      * 087-087 OCOR-07 = PONTO DE VENDA NAO NUMERICO OU ZERADO
      * 088-088 OCOR-08 = CNPJ/CPF DO ESTAB. NAO NUMERICO OU ZERADO
      * 089-089 OCOR-09 = FILIAL DO CNPJ ZERADA COM TIPO PESSOA = 'J'
      * 090-090 OCOR-10 = TIPO DE PESSOA DIFERENTE DE 'F' E 'J'
      * 091-091 OCOR-11 = AGENCIA DA CONTA CORRENTE NAO NUMERICA/ZERADA
      * 092-092 OCOR-12 = NUMERO DA CONTA CORRENTE NAO NUMERICO/ZERADO
      * 093-093 OCOR-13 = DIGITO DA CONTA CORRENTE INVALIDO
      * 094-094 OCOR-14 = NUMERO SEQUENCIAL NAO NUMERICO OU ZERADO
      * 095-095 OCOR-15 = INDICATIVO BASE DIFERENTE DE 'L' E 'E'
      * 096-096 OCOR-16 = TIPO DE ALTERACAO DIFERENTE DE '1'
      * 097-097 OCOR-17 = DATA DE SOLICITACAO DA ALTERACAO INVALIDA
      * 098-098 OCOR-18 = TIPO DE PROCESSO DIFERENTE DE '1'
      * 099-099 OCOR-19 = TOTAL DE REGISTROS DO TRAILER NAO BATE
      * 100-101 QUANTIDADE DE CAMPOS COM ERRO
      * 102-110 RESERVA PARA USO FUTURO
      *----------------------------------------------------------------*
