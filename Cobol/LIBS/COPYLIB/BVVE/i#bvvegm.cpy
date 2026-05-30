      *----------------------------------------------------------------*
      *  SISTEMA : BVVE - CARTAO DE CREDITO / VENDEDOR                 *
      *  ARQUIVO : MOVIMENTO DE RETORNO DE SOLICITACOES DE             *
      *            AFILIACOES ENVIADAS 'A REDECARD                     *
      *  LRECL   : 250 (FB)                                            *
      *  NOME INC: I#BVVEGM                                            *
      *  DATA    : 24/07/2008                                          *
      *----------------------------------------------------------------*
       01  BVVEGM-REG-HEADER.
           05  BVVEGMH-TIPO-REG               PIC X(001).
           05  BVVEGMH-NRO-LOTE-X.
               10  BVVEGMH-NRO-LOTE           PIC 9(006).
           05  BVVEGMH-DATA-LOTE-X.
               10  BVVEGMH-DATA-LOTE          PIC 9(008).
           05  BVVEGMH-HHMMSS-LOTE.
               10  BVVEGMH-HORA-LOTE-X.
                   15  BVVEGMH-HH-LOTE        PIC 9(002).
               10  BVVEGMH-2-PONTOS1          PIC X(001).
               10  BVVEGMH-MINU-LOTE-X.
                   15  BVVEGMH-MM-LOTE        PIC 9(002).
               10  BVVEGMH-2-PONTOS2          PIC X(001).
               10  BVVEGMH-SEGU-LOTE-X.
                   15  BVVEGMH-SS-LOTE        PIC 9(002).
           05  BVVEGMH-IDENT-EMISSOR-X.
               10  BVVEGMH-IDENT-EMISSOR      PIC 9(003).
           05  BVVEGMH-FIXO                   PIC X(023).
           05  FILLER                         PIC X(201).
       01  BVVEGM-REG-DETALHE.
           05  BVVEGMD-TIPO-REG               PIC X(001).
           05  BVVEGMD-NRO-LOTE-X.
               10  BVVEGMD-NRO-LOTE           PIC 9(006).
           05  BVVEGMD-DATA-LOTE-X.
               10  BVVEGMD-DATA-LOTE          PIC 9(008).
           05  BVVEGMD-HHMMSS-LOTE.
               10  BVVEGMD-HORA-LOTE-X.
                   15  BVVEGMD-HH-LOTE        PIC 9(002).
               10  BVVEGMD-2-PONTOS1          PIC X(001).
               10  BVVEGMD-MINU-LOTE-X.
                   15  BVVEGMD-MM-LOTE        PIC 9(002).
               10  BVVEGMD-2-PONTOS2          PIC X(001).
               10  BVVEGMD-SEGU-LOTE-X.
                   15  BVVEGMD-SS-LOTE        PIC 9(002).
           05  BVVEGMD-IDENT-EMISSOR-X.
               10  BVVEGMD-IDENT-EMISSOR      PIC 9(003).
           05  BVVEGMD-NUMERO-REG-X.
               10  BVVEGMD-NUMERO-REG         PIC 9(006).
           05  BVVEGMD-TIPO-PESSOA            PIC X(001).
           05  BVVEGMD-CNPJ-ESTAB.
               10  BVVEGMD-ECNPJ-NRO          PIC 9(009).
               10  BVVEGMD-ECNPJ-FILIAL       PIC 9(004).
               10  BVVEGMD-ECNPJ-CTRL         PIC 9(002).
           05  BVVEGMD-CPF-ESTAB REDEFINES BVVEGMD-CNPJ-ESTAB.
               10  BVVEGMD-ECPF-ZEROS         PIC 9(004).
               10  BVVEGMD-ECPF-NRO           PIC 9(009).
               10  BVVEGMD-ECPF-CTRL          PIC 9(002).
           05  BVVEGMD-INDIC-FILIACAO         PIC X(001).
           05  BVVEGMD-CODIGO-MOTIVO-X.
               10  BVVEGMD-CODIGO-MOTIVO      PIC 9(005).
           05  BVVEGMD-NUMERO-HE-X.
               10  BVVEGMD-NUMERO-HE          PIC 9(009).
           05  BVVEGMD-ABERTURA-SOLIC-X.
               10  BVVEGMD-ABERTURA-SOLIC     PIC 9(008).
           05  BVVEGMD-DATA-FILIACAO-X.
               10  BVVEGMD-DATA-FILIACAO      PIC 9(008).
           05  BVVEGMD-PREVISAO-INSTAL-X.
               10  BVVEGMD-PREVISAO-INSTAL    PIC 9(008).
           05  BVVEGMD-INSTAL-MAQUINETA-X.
               10  BVVEGMD-INSTAL-MAQUINETA   PIC 9(008).
           05  BVVEGMD-PREVISAO-TECNO-X.
               10  BVVEGMD-PREVISAO-TECNO     PIC 9(008).
           05  BVVEGMD-INSTAL-TECNO-X.
               10  BVVEGMD-INSTAL-TECNO       PIC 9(008).
           05  BVVEGMD-DESCRICAO-MOTIVO       PIC X(060).
           05  BVVEGMD-PONTO-VENDA-X.
               10  BVVEGMD-PONTO-VENDA        PIC 9(009).
           05  BVVEGMD-INDIC-INTERNET         PIC X(001).
           05  BVVEGMD-RAZAO-SOCIAL           PIC X(027).
           05  BVVEGMD-CODIGO-CANAL           PIC X(002).
           05  BVVEGMD-AGENCIA-FILIACAO       PIC X(004).
           05  BVVEGMD-CNPJ-VENDEDOR.
               10  BVVEGMD-VCNPJ-NRO          PIC 9(009).
               10  BVVEGMD-VCNPJ-FILIAL       PIC 9(004).
               10  BVVEGMD-VCNPJ-CTRL         PIC 9(002).
           05  BVVEGMD-CPF-VENDEDOR REDEFINES BVVEGMD-CNPJ-VENDEDOR.
               10  BVVEGMD-VCPF-ZEROS         PIC 9(004).
               10  BVVEGMD-VCPF-NRO           PIC 9(009).
               10  BVVEGMD-VCPF-CTRL          PIC 9(002).
           05  BVVEGMD-TIPO-MOVIMENTO         PIC X(001).
           05  BVVEGMD-CODIGO-CELULA          PIC X(005).
           05  FILLER                         PIC X(015).
       01  BVVEGM-REG-TRAILER.
           05  BVVEGMT-TIPO-REG               PIC X(001).
           05  BVVEGMT-NRO-LOTE-X.
               10  BVVEGMT-NRO-LOTE           PIC 9(006).
           05  BVVEGMT-DATA-LOTE-X.
               10  BVVEGMT-DATA-LOTE          PIC 9(008).
           05  BVVEGMT-HHMMSS-LOTE.
               10  BVVEGMT-HORA-LOTE-X.
                   15  BVVEGMT-HH-LOTE        PIC 9(002).
               10  BVVEGMT-2-PONTOS1          PIC X(001).
               10  BVVEGMT-MINU-LOTE-X.
                   15  BVVEGMT-MM-LOTE        PIC 9(002).
               10  BVVEGMT-2-PONTOS2          PIC X(001).
               10  BVVEGMT-SEGU-LOTE-X.
                   15  BVVEGMT-SS-LOTE        PIC 9(002).
           05  BVVEGMT-IDENT-EMISSOR-X.
               10  BVVEGMT-IDENT-EMISSOR      PIC 9(003).
           05  BVVEGMT-TOTAL-REG-X.
               10  BVVEGMT-TOTAL-REG          PIC 9(006).
           05  BVVEGMT-FIXO                   PIC X(017).
           05  FILLER                         PIC X(201).
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
