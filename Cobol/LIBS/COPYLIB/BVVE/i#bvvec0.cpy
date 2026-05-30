      *================================================================*
      *       AFILIACAO ELETRONICA DE ESTABELECIMENTOS - CIELO         *
      *                                                                *
      * MENSAGEM ONLINE CIELO - LREL: 1920                             *
      *  ENVIO   = 045                                                 *
      *  RETORNO = 145                                                 *
      *================================================================*
       01  BVVEC0-MSG-CIELO.
           05  BVVEC0-COD-MSG              PIC 9(03).
      *        045 - BANCO ENVIA MSG A CIELO
      *        145 - CIELO ENVIA MSG AO BANCO
           05  BVVEC0-INST-FINANCEIRA      PIC 9(04).
           05  BVVEC0-TERMINAL             PIC X(04).
           05  BVVEC0-FILLER-01            PIC X(19).
           05  BVVEC0-COD-ERRO             PIC 9(02).
           05  BVVEC0-RET-BANCO            PIC 9(02).
           05  BVVEC0-BANCO                PIC 9(03).
           05  BVVEC0-ESTABELECIMENTO      PIC 9(10).
           05  BVVEC0-RET-RAZAO            PIC 9(02).
           05  BVVEC0-RAZAO-SOCIAL         PIC X(32).

      * DADOS ENDERECO DE CORRESPONDENCIA -----------------------------*
           05  BVVEC0-RET-END-CORR         PIC 9(02).
           05  BVVEC0-ENDERECO-CORR        PIC X(32).
           05  BVVEC0-COMPL-CORR           PIC X(32).
           05  BVVEC0-RET-CIDADE-CORR      PIC 9(02).
           05  BVVEC0-CIDADE-CORR          PIC X(28).
           05  BVVEC0-RET-UF-CORR          PIC 9(02).
           05  BVVEC0-UF-CORR              PIC X(02).
           05  BVVEC0-RET-CEP1-CORR        PIC 9(02).
           05  BVVEC0-CEP1-CORR            PIC 9(08).

           05  BVVEC0-RET-CNPJ-CPF         PIC 9(02).
           05  BVVEC0-CNPJ-CPF             PIC 9(15).
           05  BVVEC0-RET-INSC-ESTAD       PIC 9(02).
           05  BVVEC0-INSC-ESTAD           PIC 9(15).
           05  BVVEC0-RET-NOME-FANT        PIC 9(02).
           05  BVVEC0-NOME-FANT            PIC X(32).

      * DADOS ENDERECO DO ESTABELECIMENTO -----------------------------*
           05  BVVEC0-RET-END-ESTB         PIC 9(02).
           05  BVVEC0-ENDERECO-ESTB        PIC X(32).
           05  BVVEC0-COMPL-ESTB           PIC X(32).
           05  BVVEC0-RET-CIDADE-ESTB      PIC 9(02).
           05  BVVEC0-CIDADE-ESTB          PIC X(28).
           05  BVVEC0-RET-UF-ESTB          PIC 9(02).
           05  BVVEC0-UF-ESTB              PIC X(02).
           05  BVVEC0-RET-CEP1-ESTB        PIC 9(02).
           05  BVVEC0-CEP1-ESTB            PIC 9(08).

           05  BVVEC0-RET-NOME-PLAQ        PIC 9(02).
           05  BVVEC0-NOME-PLAQUETA        PIC X(22).
           05  BVVEC0-RET-CONTATO          PIC 9(02).
           05  BVVEC0-CONTATO              PIC X(32).
           05  BVVEC0-FILLER-02            PIC X(32).
           05  BVVEC0-FILLER-03            PIC 9(02).
           05  BVVEC0-FILLER-04            PIC 9(10).
           05  BVVEC0-RET-COD-RAMO         PIC 9(02).
           05  BVVEC0-COD-RAMO             PIC 9(05).
           05  BVVEC0-RET-AGENCIA          PIC 9(02).
           05  BVVEC0-AGENCIA              PIC 9(05).
           05  BVVEC0-RET-CONTA            PIC 9(02).
           05  BVVEC0-CONTA                PIC X(14).

           05  BVVEC0-RET-NM-PROP1         PIC 9(02).
           05  BVVEC0-NM-PROP1             PIC X(32).
           05  BVVEC0-RET-CPF-PROP1        PIC 9(02).
           05  BVVEC0-CPF-PROP1            PIC 9(15).
           05  BVVEC0-RET-DT-NASC-P1       PIC 9(02).
           05  BVVEC0-DT-NASC-P1           PIC 9(06).

           05  BVVEC0-RET-NM-PROP2         PIC 9(02).
           05  BVVEC0-NM-PROP2             PIC X(32).
           05  BVVEC0-RET-CPF-PROP2        PIC 9(02).
           05  BVVEC0-CPF-PROP2            PIC 9(15).
           05  BVVEC0-RET-DT-NASC-P2       PIC 9(02).
           05  BVVEC0-DT-NASC-P2           PIC 9(06).

           05  BVVEC0-RET-NM-PROP3         PIC 9(02).
           05  BVVEC0-NM-PROP3             PIC X(32).
           05  BVVEC0-RET-CPF-PROP3        PIC 9(02).
           05  BVVEC0-CPF-PROP3            PIC 9(15).
           05  BVVEC0-RET-DT-NASC-P3       PIC 9(02).
           05  BVVEC0-DT-NASC-P3           PIC 9(06).

           05  BVVEC0-RET-TP-PSSOA         PIC 9(02).
           05  BVVEC0-TP-PSSOA             PIC X(01).
           05  BVVEC0-RET-CTRO-COMPRAS     PIC 9(02).
           05  BVVEC0-CTRO-COMPRAS         PIC X(01).
           05  BVVEC0-RET-TP-TERMINAL      PIC 9(02).
           05  BVVEC0-TP-TERMINAL          PIC X(01).
           05  BVVEC0-RET-COMPL-MMC        PIC 9(02).
           05  BVVEC0-COMPL-MMC            PIC X(01).
           05  BVVEC0-FILLER-05            PIC 9(02).
           05  BVVEC0-FILLER-06            PIC X(01).

           05  BVVEC0-RET-DDD-TELEFONE     PIC 9(02).
           05  BVVEC0-DDD-TELEFONE         PIC 9(04).
           05  BVVEC0-RET-TELEFONE         PIC 9(02).
           05  BVVEC0-TELEFONE             PIC 9(09).
           05  BVVEC0-RET-DDD-FAX          PIC 9(02).
           05  BVVEC0-DDD-FAX              PIC 9(04).
           05  BVVEC0-RET-FAX              PIC 9(02).
           05  BVVEC0-FAX                  PIC 9(09).

           05  BVVEC0-RET-MEI              PIC 9(02).
           05  BVVEC0-MEI                  PIC X(01).
           05  BVVEC0-FILLER-07            PIC X(449).
           05  BVVEC0-DADOS-INFORMAIS      PIC X(291).

           05  BVVEC0-DT-AFILIACAO-VV      PIC 9(08).
           05  BVVEC0-RET-BCO-VV           PIC 9(02).
           05  BVVEC0-BCO-VV               PIC 9(04).
           05  BVVEC0-RET-AGE-VV           PIC 9(02).
           05  BVVEC0-AGE-VV               PIC 9(05).
           05  BVVEC0-RET-CTA-VV           PIC 9(02).
           05  BVVEC0-CTA-VV               PIC X(14).
           05  BVVEC0-RET-PROTOCOLO        PIC 9(02).
           05  BVVEC0-PROTOCOLO            PIC X(01).
           05  BVVEC0-RET-DUPL-CADASTRO    PIC 9(02).
           05  BVVEC0-DUPL-CADASTRO        PIC X(01).
           05  BVVEC0-RET-OPER-SAUDE       PIC 9(02).
           05  BVVEC0-OPER-SAUDE           PIC X(01).
           05  BVVEC0-FILLER-08            PIC X(17).
           05  BVVEC0-FILLER-09            PIC X(22).
           05  BVVEC0-RET-TROCO-FACIL      PIC 9(02).
           05  BVVEC0-TROCO-FACIL          PIC X(01).
           05  BVVEC0-RET-TROCO-TAXA       PIC 9(02).
           05  BVVEC0-TROCO-TAXA           PIC 9(03)V99.
           05  BVVEC0-RET-TROCO-PRAZO      PIC 9(02).
           05  BVVEC0-TROCO-PRAZO          PIC 9(03).

           05  BVVEC0-RET-FLEX-CAR         PIC 9(02).
           05  BVVEC0-FLEX-CAR             PIC X(01).
           05  BVVEC0-RET-FLEX-TAXA        PIC 9(02).
           05  BVVEC0-FLEX-TAXA            PIC 9(03)V99.
           05  BVVEC0-RET-FLEX-PRAZO       PIC 9(02).
           05  BVVEC0-FLEX-PRAZO           PIC 9(03).
           05  BVVEC0-FILLER-10            PIC X(24).
           05  BVVEC0-EMAIL                PIC X(60).
           05  BVVEC0-FILLER-11            PIC X(220).
           05  BVVEC0-FILLER-12            PIC X(31).
