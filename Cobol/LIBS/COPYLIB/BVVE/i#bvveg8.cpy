      *----------------------------------------------------------------*
      *  SISTEMA : BVVE - CARTAO DE CREDITO / VENDEDOR                 *
      *  ARQUIVO : REGISTROS DE RETORNO DE SOLICITACOES DE AFILIACOES  *
      *            ENVIADAS 'A REDECARD  QUE  APRESENTARAM  ERROS  NA  *
      *            CONSISTENCIA FISICA.                                *
      *  LRECL   : 300 (FB)                                            *
      *  NOME INC: I#BVVEG8                                            *
      *  DATA    : 06/08/2008                                          *
      *----------------------------------------------------------------*
       01  BVVEG8-REG.
           05  BVVEG8-HEADER.
               10  BVVEG8H-TIPO-REG               PIC X(001).
               10  BVVEG8H-NRO-LOTE-X.
                   15  BVVEG8H-NRO-LOTE           PIC 9(006).
               10  BVVEG8H-DATA-LOTE-X.
                   15  BVVEG8H-DATA-LOTE          PIC 9(008).
               10  BVVEG8H-HHMMSS-LOTE.
                   15  BVVEG8H-HORA-LOTE-X.
                       20  BVVEG8H-HH-LOTE        PIC 9(002).
                   15  BVVEG8H-2-PONTOS1          PIC X(001).
                   15  BVVEG8H-MINU-LOTE-X.
                       20  BVVEG8H-MM-LOTE        PIC 9(002).
                   15  BVVEG8H-2-PONTOS2          PIC X(001).
                   15  BVVEG8H-SEGU-LOTE-X.
                       20  BVVEG8H-SS-LOTE        PIC 9(002).
               10  BVVEG8H-IDENT-EMISSOR-X.
                   15  BVVEG8H-IDENT-EMISSOR      PIC 9(003).
               10  BVVEG8H-FIXO                   PIC X(023).
               10  FILLER                         PIC X(201).
           05  BVVEG8-DETALHE  REDEFINES  BVVEG8-HEADER.
               10  BVVEG8D-TIPO-REG               PIC X(001).
               10  BVVEG8D-NRO-LOTE-X.
                   15  BVVEG8D-NRO-LOTE           PIC 9(006).
               10  BVVEG8D-DATA-LOTE-X.
                   15  BVVEG8D-DATA-LOTE          PIC 9(008).
               10  BVVEG8D-HHMMSS-LOTE.
                   15  BVVEG8D-HORA-LOTE-X.
                       20  BVVEG8D-HH-LOTE        PIC 9(002).
                   15  BVVEG8D-2-PONTOS1          PIC X(001).
                   15  BVVEG8D-MINU-LOTE-X.
                       20  BVVEG8D-MM-LOTE        PIC 9(002).
                   15  BVVEG8D-2-PONTOS2          PIC X(001).
                   15  BVVEG8D-SEGU-LOTE-X.
                       20  BVVEG8D-SS-LOTE        PIC 9(002).
               10  BVVEG8D-IDENT-EMISSOR-X.
                   15  BVVEG8D-IDENT-EMISSOR      PIC 9(003).
               10  BVVEG8D-NUMERO-REG-X.
                   15  BVVEG8D-NUMERO-REG         PIC 9(006).
               10  BVVEG8D-TIPO-PESSOA            PIC X(001).
               10  BVVEG8D-CNPJ-ESTAB.
                   15  BVVEG8D-ECNPJ-NRO          PIC 9(009).
                   15  BVVEG8D-ECNPJ-FILIAL       PIC 9(004).
                   15  BVVEG8D-ECNPJ-CTRL         PIC 9(002).
               10  BVVEG8D-CPF-ESTAB REDEFINES BVVEG8D-CNPJ-ESTAB.
                   15  BVVEG8D-ECPF-ZEROS         PIC 9(004).
                   15  BVVEG8D-ECPF-NRO           PIC 9(009).
                   15  BVVEG8D-ECPF-CTRL          PIC 9(002).
               10  BVVEG8D-INDIC-FILIACAO         PIC X(001).
               10  BVVEG8D-CODIGO-MOTIVO-X.
                   15  BVVEG8D-CODIGO-MOTIVO      PIC 9(005).
               10  BVVEG8D-NUMERO-HE-X.
                   15  BVVEG8D-NUMERO-HE          PIC 9(009).
               10  BVVEG8D-ABERTURA-SOLIC-X.
                   15  BVVEG8D-ABERTURA-SOLIC     PIC 9(008).
               10  BVVEG8D-DATA-FILIACAO-X.
                   15  BVVEG8D-DATA-FILIACAO      PIC 9(008).
               10  BVVEG8D-PREVISAO-INSTAL-X.
                   15  BVVEG8D-PREVISAO-INSTAL    PIC 9(008).
               10  BVVEG8D-INSTAL-MAQUINETA-X.
                   15  BVVEG8D-INSTAL-MAQUINETA   PIC 9(008).
               10  BVVEG8D-PREVISAO-TECNO-X.
                   15  BVVEG8D-PREVISAO-TECNO     PIC 9(008).
               10  BVVEG8D-INSTAL-TECNO-X.
                   15  BVVEG8D-INSTAL-TECNO       PIC 9(008).
               10  BVVEG8D-DESCRICAO-MOTIVO       PIC X(060).
               10  BVVEG8D-PONTO-VENDA-X.
                   15  BVVEG8D-PONTO-VENDA        PIC 9(009).
               10  BVVEG8D-INDIC-INTERNET         PIC X(001).
               10  BVVEG8D-RAZAO-SOCIAL           PIC X(027).
               10  BVVEG8D-CODIGO-CANAL           PIC X(002).
               10  BVVEG8D-AGENCIA-FILIACAO       PIC X(004).
               10  BVVEG8D-CNPJ-VENDEDOR.
                   15  BVVEG8D-VCNPJ-NRO          PIC 9(009).
                   15  BVVEG8D-VCNPJ-FILIAL       PIC 9(004).
                   15  BVVEG8D-VCNPJ-CTRL         PIC 9(002).
               10  BVVEG8D-CPF-VENDEDOR REDEFINES BVVEG8D-CNPJ-VENDEDOR.
                   15  BVVEG8D-VCPF-ZEROS         PIC 9(004).
                   15  BVVEG8D-VCPF-NRO           PIC 9(009).
                   15  BVVEG8D-VCPF-CTRL          PIC 9(002).
               10  BVVEG8D-TIPO-MOVIMENTO         PIC X(001).
               10  BVVEG8D-CODIGO-CELULA          PIC X(005).
               10  FILLER                         PIC X(015).
           05  BVVEG8-TRAILER  REDEFINES  BVVEG8-HEADER.
               10  BVVEG8T-TIPO-REG               PIC X(001).
               10  BVVEG8T-NRO-LOTE-X.
                   15  BVVEG8T-NRO-LOTE           PIC 9(006).
               10  BVVEG8T-DATA-LOTE-X.
                   15  BVVEG8T-DATA-LOTE          PIC 9(008).
               10  BVVEG8T-HHMMSS-LOTE.
                   15  BVVEG8T-HORA-LOTE-X.
                       20  BVVEG8T-HH-LOTE        PIC 9(002).
                   15  BVVEG8T-2-PONTOS1          PIC X(001).
                   15  BVVEG8T-MINU-LOTE-X.
                       20  BVVEG8T-MM-LOTE        PIC 9(002).
                   15  BVVEG8T-2-PONTOS2          PIC X(001).
                   15  BVVEG8T-SEGU-LOTE-X.
                       20  BVVEG8T-SS-LOTE        PIC 9(002).
               10  BVVEG8T-IDENT-EMISSOR-X.
                   15  BVVEG8T-IDENT-EMISSOR      PIC 9(003).
               10  BVVEG8T-TOTAL-REG-X.
                   15  BVVEG8T-TOTAL-REG          PIC 9(006).
               10  BVVEG8T-FIXO                   PIC X(017).
               10  FILLER                         PIC X(201).
           05  BVVEG8-TAB-ERRO                    PIC 9(001) OCCURS 29.
           05  BVVEG8-QTDE-ERROS                  PIC 9(003) COMP-3.
           05  FILLER                             PIC X(019).
      *----------------------------------------------------------------*
      * 001-001 TIPO DE REGISTRO = 1 (HEADER)
      * 002-007 NUMERO DO LOTE
      * 008-015 DATA DE GERACAO DO LOTE
      * 016-023 HORARIO DE GERACAO DO LOTE (HH:MM:SS)
      * 024-026 IDENTIFICACAO DO EMISSOR (CODIGO DO BANCO)
      * 027-049 CONTEUDO FIXO "00000000000000000000000"
      * 050-250 RESERVA PARA USO FUTURO
      *----------------------------------------------------------------*
      * 001-001 TIPO DE REGISTRO = 5 (FILIACAO) - (DETALHE)
      * 002-007 NUMERO DO LOTE
      * 008-015 DATA DE GERACAO DO LOTE
      * 016-023 HORARIO DE GERACAO DO LOTE (HH:MM:SS)
      * 024-026 IDENTIFICACAO DO EMISSOR (CODIGO DO BANCO)
      * 027-032 NUMERO DO REGISTRO
      * 033-033 TIPO DE PESSOA (F=FISICA J=JURIDICA)
      * 034-048 CNPJ/CPF DO ESTABELECIMENTO
      * 049-049 INDICADOR DE FILIACAO (STATUS REDECARD)
      * 050-054 CODIGO DO MOTIVO DO ERRO
      * 055-063 NUMERO DO HE
      * 064-071 DATA DE ABERTURA DA SOLICITACAO DE AFILIACAO
      * 072-079 DATA DE FILIACAO
      * 080-087 DATA DE PREVISAO DE INSTALACAO DA MAQUINETA
      * 088-095 DATA EFETIVA DE INSTALACAO DA MAQUINETA
      * 096-103 DATA PREVISTA PARA DISPONIBILIZACAO DA TECNOLOGIA
      * 104-111 DATA EFETIVA DE DISPONIBILIZACAO DA TECNOLOGIA
      * 112-171 DESCRICAO DO MOTIVO DO ERRO
      * 172-180 NUMERO DO PONTO DE VENDA/ESTABELECIMENTO
      * 181-181 INDICATIVO DE PROPOSTA GERADA PELA INTERNET
      * 182-208 RAZAO SOCIAL DO ESTABELECIMENTO
      * 209-210 CODIGO DO CANAL
      * 211-214 CODIGO DA AGENCIA DE FILIACAO
      * 215-229 CNPJ/CPF DA EMPRESA CONTRATADA
      * 230-230 CODIGO DO TIPO DE MOVIMENTO (S=SERVICO E=EMISSOR)
      * 231-235 CODIGO DA CELULA
      * 236-250 RESERVA PARA USO FUTURO
      *----------------------------------------------------------------*
      * 001-001 TIPO DE REGISTRO = 8 (TRAILER)
      * 002-007 NUMERO DO LOTE
      * 008-015 DATA DE GERACAO DO LOTE
      * 016-023 HORARIO DE GERACAO DO LOTE (HH:MM:SS)
      * 024-026 IDENTIFICACAO DO EMISSOR (CODIGO DO BANCO)
      * 027-032 TOTAL DE REGISTROS (EXCETO HEADER E TRAILER)
      * 033-049 CONTEUDO FIXO "00000000000000000000000"
      * 050-250 RESERVA PARA USO FUTURO
      *----------------------------------------------------------------*
      * 251-279 IDENTIFICADORES DE ERROS POR CAMPO 0=OK 1=COM ERRO
      *         APESAR DE PREVISTAS, AS OCORRENCIAS 17 A 29 NAO SERAO
      *         ASSINALADAS, POR ENQUANTO.
      * 251-251 OCOR-01 = TIPO DE REGISTRO INVALIDO
      * 252-252 OCOR-02 = LOTE NAO NUMERICO OU ZERADO
      * 253-253 OCOR-03 = DATA DO LOTE INVALIDA
      * 254-254 OCOR-04 = HORARIO DO LOTE INVALIDO
      * 255-255 OCOR-05 = EMISSOR DIFERENTE DE 237
      * 256-256 OCOR-06 = NUMERO DE REGISTRO NAO NUMERICO
      * 257-257 OCOR-07 = TIPO DE PESSOA DIFERENTE DE 'F' E 'J'
      * 258-258 OCOR-08 = CNPJ/CPF DO ESTAB. NAO NUMERICO OU ZERADO
      * 259-259 OCOR-09 = FILIAL DO CNPJ ZERADA COM TIPO PESSOA = 'J'
      * 260-260 OCOR-10 = INDICADOR DE FILIACAO NAO ESTA ENTRE A-Z
      * 261-261 OCOR-11 = CODIGO DE MOTIVO INVALIDO
      * 262-262 OCOR-12 = DATA DE FILIACAO INVALIDA
      * 263-263 OCOR-13 = DESCRICAO MOTIVO INVALIDA P/ COD.MOTIVO > 0
      * 264-264 OCOR-14 = NUMERO DE PONTO DE VENDA INVALIDO
      * 265-265 OCOR-15 = RAZAO SOCIAL INVALIDA
      * 266-266 OCOR-16 = TIPO DE MOVIMENTO DIFERENTE DE 'E' E 'S'
      * 267-267 OCOR-17 = NUMERO HE NAO NUMERICO
      * 268-268 OCOR-18 = DATA DE ABERTURA DA SOLICITACAO INVALIDA
      * 269-269 OCOR-19 = DATA PREVISAO INSTAL.MAQUINETA INVALIDA
      * 270-270 OCOR-20 = DATA INSTAL.MAQUINETA INVALIDA
      * 271-271 OCOR-21 = DATA DE PREVISAO INSTAL.TECNOLOGIA INVALIDA
      * 272-272 OCOR-22 = DATA INSTAL.TECNOLOGIA INVALIDA
      * 273-273 OCOR-23 = INDIC.INTERNET INVALIDO
      * 274-274 OCOR-24 = CODIGO DE CANAL INVALIDO
      * 275-275 OCOR-25 = AGENCIA DE FILIACAO INVALIDA
      * 276-276 OCOR-26 = CNPJ/CPF DO VENDEDOR INVALIDO
      * 277-277 OCOR-27 = FILIAL DO CNPJ DO VENDEDOR INVALIDA
      * 278-278 OCOR-28 = CODIGO DE CELULA INVALIDO
      * 279-279 OCOR-29 = TOTAL DE REGISTROS DO TRAILER NAO BATE
      * 280-281 QUANTIDADE DE CAMPOS COM ERRO
      * 282-300 RESERVA PARA USO FUTURO
      *----------------------------------------------------------------*
