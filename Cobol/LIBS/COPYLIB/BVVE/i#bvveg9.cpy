      *----------------------------------------------------------------*
      *  SISTEMA : BVVE - CARTAO DE CREDITO / VENDEDOR                 *
      *  ARQUIVO : REGISTROS DO RETORNO DE  SOLICITACOES DE TRANSFE-   *
      *            RENCIA DE DOMICILIO BANCARIO ENVIADAS 'A REDECARD
      *            QUE APRESENTARAM ERROS NA CONSISTENCIA FISICA.      *
      *  LRECL   : 240 (FB)                                            *
      *  NOME INC: I#BVVEG9                                            *
      *  DATA    : 07/08/2008                                          *
      *----------------------------------------------------------------*
       01  BVVEG9-REG.
           05  BVVEG9-HEADER.
               10  BVVEG9H-TIPO-REG-X.
                   15  BVVEG9H-TIPO-REG           PIC 9(001).
               10  BVVEG9H-BANCO-ENVIO-X.
                   15  BVVEG9H-BANCO-ENVIO        PIC 9(003).
               10  BVVEG9H-DATA-ENVIO-X.
                   15  BVVEG9H-DATA-ENVIO         PIC 9(008).
               10  BVVEG9H-HORA-ENVIO-X.
                   15  BVVEG9H-HORA-ENVIO         PIC 9(006).
               10  BVVEG9H-VERSAO-ARQ-X.
                   15  BVVEG9H-VERSAO-ARQ         PIC 9(009).
               10  FILLER                         PIC X(173).
           05  BVVEG9-DETALHE  REDEFINES  BVVEG9-HEADER.
               10  BVVEG9D-TIPO-REG-X.
                   15  BVVEG9D-TIPO-REG           PIC 9(001).
               10  BVVEG9D-BANCO-ENVIO-X.
                   15  BVVEG9D-BANCO-ENVIO        PIC 9(003).
               10  BVVEG9D-DATA-ENVIO-X.
                   15  BVVEG9D-DATA-ENVIO         PIC 9(008).
               10  BVVEG9D-RAZAO-SOCIAL           PIC X(028).
               10  BVVEG9D-PONTO-VENDA-X.
                   15  BVVEG9D-PONTO-VENDA        PIC 9(009).
               10  BVVEG9D-CNPJ.
                   15  BVVEG9D-CNPJ-NRO           PIC 9(008).
                   15  BVVEG9D-CNPJ-FILIAL        PIC 9(004).
                   15  BVVEG9D-CNPJ-CTRL          PIC 9(002).
               10  BVVEG9D-CPF REDEFINES BVVEG9D-CNPJ.
                   15  FILLER                     PIC X(003).
                   15  BVVEG9D-CPF-NRO            PIC 9(009).
                   15  BVVEG9D-CPF-CTRL           PIC 9(002).
               10  BVVEG9D-TIPO-PESSOA            PIC X(001).
               10  BVVEG9D-OPERACAO               PIC X(001).
               10  BVVEG9D-BANCO-X.
                   15  BVVEG9D-BANCO              PIC 9(003).
               10  BVVEG9D-AGENCIA-X.
                   15  BVVEG9D-AGENCIA            PIC 9(005).
               10  BVVEG9D-CONTA-NRO-X.
                   15  BVVEG9D-CONTA-NRO          PIC 9(014).
               10  BVVEG9D-CONTA-DIG-X.
                   15  BVVEG9D-CONTA-DIG          PIC 9(001).
               10  BVVEG9D-IND-TRAVA              PIC X(001).
               10  BVVEG9D-TIPO-PROCESSO          PIC X(001).
               10  BVVEG9D-DTENVIO-BANCO-X.
                   15  BVVEG9D-DTENVIO-BANCO      PIC 9(008).
               10  BVVEG9D-COD-RETORNO-X.
                   15  BVVEG9D-COD-RETORNO        PIC 9(005).
               10  BVVEG9D-MSG-ERRO               PIC X(060).
               10  BVVEG9D-PV-CENTRALIZ-X.
                   15  BVVEG9D-PV-CENTRALIZ       PIC 9(009).
               10  BVVEG9D-SIGLA-PRODUTO          PIC X(002).
               10  BVVEG9D-RESERVADO              PIC X(026).
           05  BVVEG9-TRAILER  REDEFINES  BVVEG9-HEADER.
               10  BVVEG9T-TIPO-REG-X.
                   15  BVVEG9T-TIPO-REG           PIC 9(001).
               10  BVVEG9T-BANCO-ENVIO-X.
                   15  BVVEG9T-BANCO-ENVIO        PIC 9(003).
               10  BVVEG9T-DATA-ENVIO-X.
                   15  BVVEG9T-DATA-ENVIO         PIC 9(008).
               10  BVVEG9T-QTDE-REG-X.
                   15  BVVEG9T-QTDE-REG           PIC 9(009).
               10  FILLER                         PIC X(179).
           05  BVVEG9-TAB-ERRO                    PIC 9(001) OCCURS 23.
           05  BVVEG9-QTDE-ERROS                  PIC 9(003) COMP-3.
           05  FILLER                             PIC X(015).
      *----------------------------------------------------------------*
      * 001-001 TIPO DE REGISTRO = 0 (ZERO) - HEADER
      * 002-004 CODIGO DO BANCO QUE ESTA ENVIANDO/RECEBENDO ARQUIVOS
      * 005-012 DATA DO ENVIO DO ARQUIVO (AAAAMMDD)
      * 013-018 HORARIO DO ENVIO DO ARQUIVO (HHMMSS)
      * 019-027 NUMERO DA VERSAO DO ARQUIVO
      * 028-200 RESERVA PARA USO FUTURO
      *----------------------------------------------------------------*
      * 001-001 TIPO DE REGISTRO = 1 (DETALHE)
      * 002-004 CODIGO DO BANCO QUE ESTA ENVIANDO/RECEBENDO ARQUIVOS
      * 005-012 DATA DO ENVIO DO ARQUIVO (AAAAMMDD)
      * 013-040 RAZAO SOCIAL DO ESTABELECIMENTO
      * 041-049 NUMERO DO PONTO DE VENDA DO ESTABELECIMENTO
      * 050-063 NUMERO DO CNPJ/CPF DO ESTABELECIMENTO
      * 064-064 TIPO DE PESSOA (F=FISICA J=JURIDICA)
      * 065-065 TIPO DE OPERACAO FINANCEIRA (C=CREDITO D=DEBITO)
      * 066-068 BANCO DA CONTA CORRENTE DO ESTABELECIMENTO
      * 069-073 AGENCIA DA CONTA CORRENTE DO ESTABELECIMENTO
      * 074-087 NUMERO DA CONTA CORRENTE DO ESTABELECIMENTO
      * 088-088 DIGITO DA CONTA CORRENTE DO ESTABELECIMENTO
      * 089-089 INDICADOR DE TRAVA DE DOMICILIO (T=TRAVADO D=DESTRAVADO)
      * 090-090 TIPO DE PROCESSO ('1' = ALTERACAO DE DOMICILIO)
      * 091-098 DATA DE ENVIO DO ARQUIVO PELO BANCO (AAAAMMDD)
      * 099-103 CODIGO DE RETORNO/ERRO
      * 104-163 MENSAGEM DE RETORNO/ERRO
      * 164-172 PONTO DE VENDA CENTRALIZADOR DE CREDITOS
      * 173-174 SIGLA DO PRODUTO
      *         CR - MASTERCARD CREDITO
      *         DB - MASTERCARD DEBITO
      *         VC - VISA CREDITO
      *         VD - VISA DEBITO
      * 175-200 RESERVA PARA USO FUTURO
      *----------------------------------------------------------------*
      * 001-001 TIPO DE REGISTRO = 9 (TRAILER)
      * 002-004 CODIGO DO BANCO QUE ESTA ENVIANDO/RECEBENDO ARQUIVOS
      * 005-012 DATA DO ENVIO DO ARQUIVO (AAAAMMDD)
      * 012-021 QUANTIDADE DE REGISTROS (INCLUSIVE HEADER E TRAILER)
      * 022-200 RESERVA PARA USO FUTURO
      *----------------------------------------------------------------*
      * 201-222 IDENTIFICADORES DE ERROS POR CAMPO   0=OK  1=COM ERRO
      *         APESAR DE PREVISTAS, AS OCORRENCIAS 21 E 23 NAO SERAO
      *         ASSINALADAS, POR ENQUANTO.
      * 201-201 OCOR-01 = TIPO DE REGISTRO INVALIDO
      * 202-202 OCOR-02 = BANCO DE ENVIO DIFERENTE DE 237
      * 203-203 OCOR-03 = DATA DE ENVIO INVALIDA
      * 204-204 OCOR-04 = HORARIO DE ENVIO INVALIDO
      * 205-205 OCOR-05 = VERSAO DE ARQUIVO NAO NUMERICA OU ZERADA
      * 206-206 OCOR-06 = RAZAO SOCIAL INVALIDA
      * 207-207 OCOR-07 = PONTO DE VENDA NAO NUMERICO OU ZERADO
      * 208-208 OCOR-08 = CNPJ/CPF DO ESTAB. NAO NUMERICO OU ZERADO
      * 209-209 OCOR-09 = FILIAL DO CNPJ ZERADA COM TIPO PESSOA = 'J'
      * 210-210 OCOR-10 = TIPO DE PESSOA DIFERENTE DE 'F' E 'J'
      * 211-211 OCOR-11 = OPERACAO DIFERENTE DE 'C' E 'D'
      * 212-212 OCOR-12 = BANCO DA CONTA CORRENTE DIFERENTE DE 237
      * 213-213 OCOR-13 = AGENCIA DA CONTA CORRENTE NAO NUMERICA/ZERADA
      * 214-214 OCOR-14 = NUMERO DA CONTA CORRENTE NAO NUMERICO/ZERADO
      * 215-215 OCOR-15 = DIGITO DA CONTA CORRENTE INVALIDO
      * 216-216 OCOR-16 = INDICADOR DE TRAVA DIFERENTE DE 'T' E 'N'
      * 217-217 OCOR-17 = TIPO DE PROCESSO DIFERENTE DE '1'
      * 218-218 OCOR-18 = DATA DE ENVIO PELO BANCO INVALIDA
      * 219-219 OCOR-19 = CODIGO DE RETORNO NAO NUMERICO
      * 220-220 OCOR-20 = MENSAGEM DE ERRO INVALIDA P/ COD.RETORNO > 0
      * 221-221 OCOR-21 = PONTO DE VENDA CENTRALIZADOR NAO NUMERICO
      * 222-222 OCOR-22 = SIGLA DO PRODUTO INVALIDA
      * 223-223 OCOR-23 = TOTAL DE REGISTROS DO TRAILER NAO BATE
      * 224-225 QUANTIDADE DE CAMPOS COM ERRO
      * 226-240 RESERVA PARA USO FUTURO
      *----------------------------------------------------------------*
