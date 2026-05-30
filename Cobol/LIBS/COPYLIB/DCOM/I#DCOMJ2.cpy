      *----------------------------------------------------------------*
      *
      *   NOTA:
      *
      *   A NAVEGACAO DE CONSULTA DE OPERACAO EH MUITO GRANDE, PORTANTO
      * A AREA DE COMUNICACAO SERA SALVA NO DBTEMPORARIO, JUNTAMENTE
      * COM OS CHAMADORES E SUAS PAGINAS, UTILIZAR ESSA AREA COMO PADRAO
      * E QUALQUER CAMPO QUE SEJA ADICIONADO NESSA AREA DEVERA SER COLO-
      * CADO APOS AO ULTIMO CAMPO.
      *----------------------------------------------------------------*

       01  WRK-AREA-COMUN-TOTAL           PIC X(1600)     VALUE SPACES.
       01  FILLER REDEFINES WRK-AREA-COMUN-TOTAL.
           05  WRK-CHAMADORES             OCCURS 15 TIMES.
               10  WRK-CHAMADOR           PIC  X(08).
               10  WRK-PAGINA             PIC  9(05).
           05  WRK-AREA-COMUM.
               10  WRK-OPCAO              PIC  9(01).
               10  WRK-TIPO-CONSULTA      PIC  9(01).
               10  WRK-TIMESTAMP          PIC  X(26).
               10  WRK-MENSAGEM-COMUM     PIC  X(79).
               10  WRK-DADOS-TEMP.
                 15  WRK-OPERACAO.
                   20  WRK-DANO-OPER-DESC   PIC  9(04).
                   20  WRK-NSEQ-OPER-DESC   PIC  9(09).
                 15  WRK-PRODUTO-LISTA      PIC  9(03).
                 15  WRK-SUBPRODUTO-LISTA   PIC  9(03).
                 15  WRK-AGENCIA-LISTA      PIC  9(05).
                 15  WRK-SITUACAO-LISTA     PIC  9(03).
                 15  WRK-AGENCIA            PIC  9(05).
                 15  WRK-DESC-AGENCIA       PIC  X(40).
                 15  WRK-PRODUTO            PIC  9(03).
                 15  WRK-DESC-PRODUTO       PIC  X(30).
                 15  WRK-SUBPRODUTO         PIC  9(03).
                 15  WRK-DESC-SPROD         PIC  X(30).
                 15  WRK-SITUACAO           PIC  9(03).
                 15  WRK-DESC-SITU          PIC  X(30).
                 15  WRK-DATA-INI-PESQ      PIC  X(10).
                 15  WRK-DATA-FIM-PESQ      PIC  X(10).
                 15  WRK-NOMECLI            PIC  X(60).
                 15  WRK-CNPJCPF            PIC  X(19).
                 15  WRK-CONTA              PIC  9(13).
                 15  WRK-SITU1              PIC  X(15).
                 15  WRK-SITU2              PIC  X(13).
                 15  WRK-SITU3              PIC  X(09).
                 15  WRK-SITU4              PIC  X(13).
                 15  WRK-SITU5              PIC  X(09).
                 15  WRK-NUM1CONS           PIC  X(04).
                 15  WRK-NUM2CONS           PIC  X(05).
                 15  WRK-NUM3CONS           PIC  X(09).
                 15  WRK-VLRTARO            PIC  X(14).
                 15  WRK-VLRTART            PIC  X(14).
                 15  WRK-DVCTFIM            PIC  X(10).
                 15  WRK-DOPERAC            PIC  X(10).
                 15  WRK-VLRJUROS           PIC  X(14).
                 15  WRK-PRZMEDIO           PIC  9(03).
                 15  WRK-VLRIOF             PIC  X(14).
                 15  WRK-QTDTIT             PIC  9(05).
                 15  WRK-VLRLIQ             PIC  X(14).
                 15  WRK-CODMEIO            PIC  9(03).
                 15  WRK-MEIOENT            PIC  X(15).
                 15  WRK-TPDES              PIC  9(03).
                 15  WRK-GARANTIA           PIC  X(20).
                 15  WRK-CARTCONS           PIC  X(03).
                 15  WRK-CONTRC             PIC  9(07).
                 15  WRK-TPTAXA             PIC  X(15).
                 15  WRK-CCNPJ-AVAL         PIC  9(09).
                 15  WRK-FILIAL-AVAL        PIC  9(05).
                 15  WRK-CCTRL-AVAL         PIC  9(02).
                 15  WRK-NOME-AVAL          PIC  X(40).
                 15  WRK-DATA-ADITAMENTO    PIC  X(10).
                 15  WRK-CCNPJ-CEDENTE      PIC  9(09).
                 15  WRK-FILIAL-CEDENTE     PIC  9(05).
                 15  WRK-CCTRL-CEDENTE      PIC  9(02).
                 15  WRK-CCNPJ-SACADO       PIC  9(09).
                 15  WRK-FILIAL-SACADO      PIC  9(05).
                 15  WRK-CCTRL-SACADO       PIC  9(02).
                 15  WRK-CONTRATO           PIC  9(09).
                 15  WRK-CONVENIO-CLIENTE   PIC  9(09).
                 15  WRK-CONVENIO-GERAL     PIC  9(09).
                 15  WRK-DESC-CONVE         PIC  X(40).
                 15  WRK-NRO-PARCELA        PIC  9(05).
                 15  WRK-DTVENC-PARCELA     PIC  X(10).
                 15  WRK-SITUAC-PARCELA     PIC  X(20).
                 15  WRK-TIMESTAMP-PARC     PIC  X(26).
                 15  WRK-TIPO-DESCONTO      PIC  9(03).
                 15  WRK-NRO-TITULO         PIC  9(05).
                 15  WRK-NRO-BORDERO        PIC  9(07).
                 15  WRK-NRO-LOTE           PIC  9(07).
                 15  WRK-NRO-CHEQ           PIC  9(07).
                 15  WRK-CPROT-LOTE-CHEQC   PIC  9(07).
                 15  WRK-BAIXA              PIC  9(03).
                 15  WRK-DESC-BAIXA         PIC  X(15).
                 15  WRK-DATA-PAGAMENTO     PIC  X(10).
                 15  WRK-TIMES-BAIXA        PIC  X(26).
                 15  WRK-NOME-SAC           PIC  X(40).
                 15  WRK-BCO-CHEQ           PIC  9(03).
                 15  WRK-AGE-CHEQ           PIC  9(05).
                 15  WRK-CTA-CHEQ           PIC  9(13).
                 15  WRK-VLR-CHEQ           PIC  9(15)V99.
                 15  WRK-SIT-CHEQ           PIC  X(20).
                 15  WRK-NRO-CORPORATIVO    PIC  9(07).
                 15  WRK-PESQ-PCELA         PIC  9(05).
                 15  WRK-CODBAIXA           PIC  X(01).
BSI001           15  WRK-COMU-OPCAO-EMIT    PIC  9(01).
BSI001           15  WRK-COMU-CBANCO-EMIT   PIC  9(03).
BSI001           15  WRK-COMU-CAGENC-EMIT   PIC  9(05).
BSI001           15  WRK-COMU-IAGENC-EMIT   PIC  X(40).
BSI001           15  WRK-COMU-CCONTA-EMIT   PIC  9(13).
BSI001           15  WRK-COMU-CCNPJ-EMIT    PIC  9(09).
BSI001           15  WRK-COMU-CFLIAL-EMIT   PIC  9(04).
BSI001           15  WRK-COMU-CCTRL-EMIT    PIC  9(02).
BSI001           15  WRK-COMU-NOME-EMIT     PIC  X(70).
BSI001           15  WRK-COMU-NPSSOA-EMIT   PIC  9(09).

  ********
