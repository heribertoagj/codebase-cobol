      ******************************************************************
      * SISTEMA : DCOM - I#DCOMCD                                      *
      * TAMANHO : 1000 BYTES                                           *
      * ARQUIVO : ARQUIVO BASE PRODUCAO - OPERACOES DESCONTO - DCOM    *
      *           ENVIADO PARA ANALISE CQR                             *
      ******************************************************************

       01  CD-REG-HEADER.
           05  CD-H-DT-GERACAO             PIC  X(010).
           05  CD-H-FILLER1                PIC  X(001).
           05  CD-H-HR-GERACAO             PIC  X(008).
           05  CD-H-FILLER2                PIC  X(001).
           05  CD-H-DS-ARQUIVO             PIC  X(977).
           05  CD-H-FILLER3                PIC  X(001).
           05  CD-H-IND-ERRO               PIC  X(001).
           05  CD-H-FILLER4                PIC  X(001).

       01  CD-REG-DETALHE.
           05  CD-D-DT-BASE                PIC  X(010).
           05  CD-D-FILLER1                PIC  X(001).
           05  CD-D-CNPJ-BASE-CED          PIC  9(009).
           05  CD-D-FILLER2                PIC  X(001).
           05  CD-D-CNPJ-FLIAL-CED         PIC  9(004).
           05  CD-D-FILLER3                PIC  X(001).
           05  CD-D-CNPJ-CTR-CED           PIC  9(002).
           05  CD-D-FILLER4                PIC  X(001).
           05  CD-D-RZ-CED                 PIC  X(080).
           05  CD-D-FILLER5                PIC  X(001).
           05  CD-D-CD-AG-CED              PIC  9(005).
           05  CD-D-FILLER6                PIC  X(001).
           05  CD-D-NR-CTA-CED             PIC  9(013).
           05  CD-D-FILLER7                PIC  X(001).
           05  CD-D-CD-PRODT               PIC  9(003).
           05  CD-D-FILLER8                PIC  X(001).
           05  CD-D-CD-SPROD               PIC  9(003).
           05  CD-D-FILLER9                PIC  X(001).
           05  CD-D-TP-BANDE               PIC  9(003).
           05  CD-D-FILLER10               PIC  X(001).
           05  CD-D-NR-OPER                PIC  9(018).
           05  CD-D-FILLER11               PIC  X(001).
           05  CD-D-NR-CONE                PIC  9(009).
           05  CD-D-FILLER12               PIC  X(001).
           05  CD-D-CD-CART                PIC  9(003).
           05  CD-D-FILLER13               PIC  X(001).
           05  CD-D-DT-OPER                PIC  X(010).
           05  CD-D-FILLER14               PIC  X(001).
           05  CD-D-CNPJ-BASE-EMITT        PIC  9(009).
           05  CD-D-FILLER15               PIC  X(001).
           05  CD-D-CNPJ-FLIAL-EMITT       PIC  9(004).
           05  CD-D-FILLER16               PIC  X(001).
           05  CD-D-CNPJ-CTR-EMITT         PIC  9(002).
           05  CD-D-FILLER17               PIC  X(001).
           05  CD-D-TP-PSSOA-EMITT         PIC  X(001).
           05  CD-D-FILLER18               PIC  X(001).
           05  CD-D-CD-NOSSO-NR            PIC  X(040).
           05  CD-D-FILLER19               PIC  X(001).
           05  CD-D-CD-SEU-NR              PIC  X(020).
           05  CD-D-FILLER20               PIC  X(001).
           05  CD-D-VCTO-DT-BOA            PIC  X(010).
           05  CD-D-FILLER21               PIC  X(001).
           05  CD-D-VL-TITLO-CHQ           PIC  99999999999999,99.
           05  CD-D-FILLER22               PIC  X(001).
           05  CD-D-ESPECIE                PIC  9(003).
           05  CD-D-FILLER23               PIC  X(001).
           05  CD-D-SIT-ANLSE-TITLO        PIC  9(001).
           05  CD-D-FILLER24               PIC  X(001).
           05  CD-D-CD-RESTR               PIC  X(001).
           05  CD-D-FILLER25               PIC  X(001).
           05  CD-D-CD-RTING-SACDO         PIC  X(001).
           05  CD-D-FILLER26               PIC  X(001).
           05  CD-D-CD-GRUPO               PIC  X(001).
           05  CD-D-FILLER27               PIC  X(001).
           05  CD-D-COCEN-CED-SACDO        PIC  99999999,99.
           05  CD-D-FILLER28               PIC  X(001).
           05  CD-D-COCEN-SACDO-OPER       PIC  99999999,99.
           05  CD-D-FILLER29               PIC  X(001).
           05  CD-D-CNPJCPF-CANC           PIC  X(001).
           05  CD-D-FILLER30               PIC  X(001).
           05  CD-D-CEP-CED-IGUAL          PIC  X(001).
           05  CD-D-FILLER31               PIC  X(001).
           05  CD-D-LIQDZ-SACDO            PIC  X(010).
           05  CD-D-FILLER32               PIC  X(001).
           05  CD-D-DESC-RESTR             PIC  X(400).
           05  CD-D-FILLER33               PIC  X(001).
           05  CD-D-DT-VCTO-FINAL          PIC  X(010).
           05  CD-D-FILLER34               PIC  X(001).
           05  CD-D-VL-OPER                PIC  99999999999999,99.
           05  CD-D-FILLER35               PIC  X(001).
           05  CD-D-VL-TARIF-OPER          PIC  99999999999999,99.
           05  CD-D-FILLER36               PIC  X(001).
           05  CD-D-VL-TARIF-TITLO         PIC  99999999999999,99.
           05  CD-D-FILLER37               PIC  X(001).
           05  CD-D-VL-JUROS               PIC  99999999999999,99.
           05  CD-D-FILLER38               PIC  X(001).
           05  CD-D-VL-IOF                 PIC  99999999999999,99.
           05  CD-D-FILLER39               PIC  X(001).
           05  CD-D-VL-LIQ                 PIC  99999999999999,99.
           05  CD-D-FILLER40               PIC  X(001).
           05  CD-D-TX-JUROS               PIC  9999,999999.
           05  CD-D-FILLER41               PIC  X(001).
           05  CD-D-TX-EFETV-MES           PIC  9999,999999.
           05  CD-D-FILLER42               PIC  X(001).
           05  CD-D-TX-EFETV-ANO           PIC  9999,999999.
           05  CD-D-FILLER43               PIC  X(001).
           05  CD-D-PRZ-MEDIO              PIC  9(007).
           05  CD-D-FILLER44               PIC  X(001).
           05  CD-D-QT-TITLO               PIC  9(011).
           05  CD-D-FILLER45               PIC  X(001).
           05  CD-D-MATR-GER               PIC  9(010).
           05  CD-D-FILLER46               PIC  X(001).
           05  CD-D-CANAL-LIBRC            PIC  X(001).
           05  CD-D-FILLER47               PIC  X(001).
           05  CD-D-FORMA-LIBRC            PIC  X(001).
           05  CD-D-FILLER48               PIC  X(001).
           05  CD-D-MEIO-ENTRD             PIC  X(001).
           05  CD-D-FILLER49               PIC  X(001).
           05  FILLER                      PIC  X(056).
           05  CD-D-FILLER50               PIC  X(001).
           05  CD-D-IND-ERRO               PIC  X(001).
           05  CD-D-FILLER51               PIC  X(001).

       01  CD-REG-TRAILLER.
           05  CD-T-QT-REGS                PIC  9(009).
           05  CD-T-FILLER1                PIC  X(001).
           05  CD-T-DS-ARQUIVO             PIC  X(987).
           05  CD-T-FILLER2                PIC  X(001).
           05  CD-T-IND-ERRO               PIC  X(001).
           05  CD-T-FILLER3                PIC  X(001).

