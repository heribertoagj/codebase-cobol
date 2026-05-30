      *----------------------------------------------------------------*
      *  SISTEMA : BVVE - CARTAO DE CREDITO / VENDEDOR                 *
      *  ARQUIVO : MOVIMENTO DE ENVIO/RETORNO DE SOLICITACOES          *
      *            DE TRANSFERENCIA DE DOMICILIO BANCARIO  DE          *
      *            ESTABELECIMENTOS                                    *
      *  LRECL   : 200 (FB)                                            *
      *  NOME INC: I#BVVEGV                                            *
      *  DATA    : 25/07/2008                                          *
      *----------------------------------------------------------------*
       01  BVVEGV-REG-HEADER.
           05  BVVEGVH-TIPO-REG-X.
               10  BVVEGVH-TIPO-REG           PIC 9(001).
           05  BVVEGVH-BANCO-ENVIO-X.
               10  BVVEGVH-BANCO-ENVIO        PIC 9(003).
           05  BVVEGVH-DATA-ENVIO-X.
               10  BVVEGVH-DATA-ENVIO         PIC 9(008).
           05  BVVEGVH-HORA-ENVIO-X.
               10  BVVEGVH-HORA-ENVIO         PIC 9(006).
           05  BVVEGVH-VERSAO-ARQ-X.
               10  BVVEGVH-VERSAO-ARQ         PIC 9(009).
           05  BVVEGVH-RESERVADO              PIC X(173).
       01  BVVEGV-REG-DETALHE.
           05  BVVEGVD-TIPO-REG-X.
               10  BVVEGVD-TIPO-REG           PIC 9(001).
           05  BVVEGVD-BANCO-ENVIO-X.
               10  BVVEGVD-BANCO-ENVIO        PIC 9(003).
           05  BVVEGVD-DATA-ENVIO-X.
               10  BVVEGVD-DATA-ENVIO         PIC 9(008).
           05  BVVEGVD-RAZAO-SOCIAL           PIC X(028).
           05  BVVEGVD-PONTO-VENDA-X.
               10  BVVEGVD-PONTO-VENDA        PIC 9(009).
           05  BVVEGVD-CNPJ.
               10  BVVEGVD-CNPJ-NRO           PIC 9(008).
               10  BVVEGVD-CNPJ-FILIAL        PIC 9(004).
               10  BVVEGVD-CNPJ-CTRL          PIC 9(002).
           05  BVVEGVD-CPF REDEFINES BVVEGVD-CNPJ.
               10  BVVEGVD-RESERVADO1         PIC 9(003).
               10  BVVEGVD-CPF-NRO            PIC 9(009).
               10  BVVEGVD-CPF-CTRL           PIC 9(002).
           05  BVVEGVD-TIPO-PESSOA            PIC X(001).
           05  BVVEGVD-OPERACAO               PIC X(001).
           05  BVVEGVD-BANCO-X.
               10  BVVEGVD-BANCO              PIC 9(003).
           05  BVVEGVD-AGENCIA-X.
               10  BVVEGVD-AGENCIA            PIC 9(005).
           05  BVVEGVD-CONTA-NRO-X.
               10  BVVEGVD-CONTA-NRO          PIC 9(014).
           05  BVVEGVD-CONTA-DIG-X.
               10  BVVEGVD-CONTA-DIG          PIC 9(001).
           05  BVVEGVD-IND-TRAVA              PIC X(001).
           05  BVVEGVD-TIPO-PROCESSO          PIC X(001).
           05  BVVEGVD-DTENVIO-BANCO-X.
               10  BVVEGVD-DTENVIO-BANCO      PIC 9(008).
           05  BVVEGVD-COD-RETORNO-X.
               10  BVVEGVD-COD-RETORNO        PIC 9(005).
           05  BVVEGVD-MSG-ERRO               PIC X(060).
           05  BVVEGVD-PV-CENTRALIZ-X.
               10  BVVEGVD-PV-CENTRALIZ       PIC 9(009).
           05  BVVEGVD-SIGLA-PRODUTO          PIC X(002).
           05  BVVEGVD-RESERVADO2             PIC X(026).
       01  BVVEGV-REG-TRAILER.
           05  BVVEGVT-TIPO-REG-X.
               10  BVVEGVT-TIPO-REG           PIC 9(001).
           05  BVVEGVT-BANCO-ENVIO-X.
               10  BVVEGVT-BANCO-ENVIO        PIC 9(003).
           05  BVVEGVT-DATA-ENVIO-X.
               10  BVVEGVT-DATA-ENVIO         PIC 9(008).
           05  BVVEGVT-QTDE-REG-X.
               10  BVVEGVT-QTDE-REG           PIC 9(009).
           05  BVVEGVT-RESERVADO              PIC X(179).
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
