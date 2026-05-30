      *----------------------------------------------------------------*
      *    COPYBOOK: DCOMWAHI                                           *
      *----------------------------------------------------------------*
      *    Comarea do serviço funcional acessado por DCOM1AHC          *
      *    Ajuste de CNPJ/CPF e FILIAL para formato alfanumérico       *
      ******************************************************************

       01 DCOMWAHI-BLOCO-ENTRADA.
           05 DCOMWAHI-E-DANO-OPER-DESC    PIC  9(04).
           05 DCOMWAHI-E-NSEQ-OPER-DESC    PIC  9(09).
           05 DCOMWAHI-E-NPCELA            PIC  9(05).
           05 DCOMWAHI-CHAVE-INICIO        PIC  X(30).
           05 DCOMWAHI-CHAVE-FINAL         PIC  X(30).

       01 DCOMWAHI-BLOCO-PAGINACAO.
           05 DCOMWAHI-INDICADOR-PAGINACAO PIC  X(01).

       01 DCOMWAHI-BLOCO-SAIDA.
           05 DCOMWAHI-S-IPSSOA-DESC-COML  PIC  X(60).
           05 DCOMWAHI-S-ISPROD-DESC-COML  PIC  X(40).
           05 DCOMWAHI-S-DANO-OPER-DESC    PIC  9(04).
           05 DCOMWAHI-S-NSEQ-OPER-DESC    PIC  9(09).
           05 DCOMWAHI-S-SIT-PAGAMENTO     PIC  X(01).
           05 DCOMWAHI-S-DVCTO-FNAL-OPER   PIC  X(10).
           05 DCOMWAHI-S-PTX-JURO-MES      PIC  9(06)V9(07).
           05 DCOMWAHI-S-PTX-JURO-ANO      PIC  9(06)V9(07).
           05 DCOMWAHI-S-VCUSTO-DESC-MES   PIC  9(15)V9(02).
           05 DCOMWAHI-S-VCUSTO-DESC-ANO   PIC  9(15)V9(02).
           05 DCOMWAHI-S-VJURO-OPER-DESC   PIC  9(15)V9(02).
           05 DCOMWAHI-S-VIOF-OPER-DESC    PIC  9(15)V9(02).
           05 DCOMWAHI-S-VTARIF-REG-TITLO  PIC  9(15)V9(02).
4S2511*      05 DCOMWAHI-S-CCNPJ-CPF       PIC  9(09).
4S2511       05 DCOMWAHI-S-CCNPJ-CPF       PIC  X(09).
4S2511*      05 DCOMWAHI-S-CFLIAL-CPF      PIC  9(04).
4S2511       05 DCOMWAHI-S-CFLIAL-CPF      PIC  X(04).
           05 DCOMWAHI-S-CCTRL-CNPJ-CPF    PIC  9(02).
           05 DCOMWAHI-S-VOPER-DESC-COML   PIC  9(15)V9(02).
           05 DCOMWAHI-S-VLR-TOT-LIQ       PIC  9(15)V9(02).
           05 DCOMWAHI-S-VLIBRC-DESC-COML  PIC  9(15)V9(02).
           05 DCOMWAHI-S-PERC-LIB          PIC  9(03)V9(02).
           05 DCOMWAHI-S-DINIC-OPER-DESC   PIC  X(10).
           05 DCOMWAHI-S-DLIBRC-DESC-COML  PIC  X(10).
           05 DCOMWAHI-S-CTPO-DESC-COML    PIC  9(03).
           05 DCOMWAHI-S-ITPO-DESC-COML    PIC  X(30).
           05 DCOMWAHI-S-LIBERACAO-CALC    PIC  9(15)V9(02).
           05 DCOMWAHI-S-SALDO-DEVEDOR     PIC  9(15)V9(02).
           05 DCOMWAHI-S-TMED-PONDE-OPER   PIC  9(03).
           05 DCOMWAHI-S-QPCELA-OPER       PIC  9(05).
           05 DCOMWAHI-S-REGISTROS         PIC  9(05).
           05 DCOMWAHI-S-PERC-REGISTROS    PIC  9(03)V9(02).
           05 DCOMWAHI-S-TRIBUTOS          PIC  9(15)V9(02).
           05 DCOMWAHI-S-PERC-TRIBUTOS     PIC  9(03)V9(02).
           05 DCOMWAHI-S-TARIFAS           PIC  9(15)V9(02).
           05 DCOMWAHI-S-PERC-TARIFAS      PIC  9(03)V9(02).
           05 DCOMWAHI-S-PGTO-SERVICOS     PIC  9(15)V9(02).
           05 DCOMWAHI-S-PERC-PGTO-SERVICOS PIC 9(03)V9(02).
           05 DCOMWAHI-S-TOPER-DESC-COML   PIC  9(03).
           05 DCOMWAHI-S-PRAZO-REMANECENTE PIC  9(03).
           05 DCOMWAHI-S-VSDO-DVDOR-ATU    PIC  9(15)V9(02).
           05 DCOMWAHI-S-VPCELA-PAGAS      PIC  9(15)V9(02).
           05 DCOMWAHI-S-VPCELA-ATRASO     PIC  9(15)V9(02).
           05 DCOMWAHI-S-VSDO-LIQUIDACAO   PIC  9(15)V9(02).
           05 DCOMWAHI-S-QTDE-OCORR        PIC  9(04).

       01 DCOMWAHI-BLOCO-SAIDA-SAC.
           05 DCOMWAHI-S-ARRAY OCCURS 100 TIMES.
              10 DCOMWAHI-S-NPCELA-NTITLO      PIC  9(05).
              10 DCOMWAHI-S-IPSSOA-SACDO       PIC  X(60).
4S2511*         10 DCOMWAHI-S-CCNPJ-CPF-SAC    PIC  9(09).
4S2511          10 DCOMWAHI-S-CCNPJ-CPF-SAC    PIC  X(09).
4S2511*         10 DCOMWAHI-S-CFLIAL-CPF-SAC   PIC  9(04).
4S2511          10 DCOMWAHI-S-CFLIAL-CPF-SAC   PIC  X(04).
              10 DCOMWAHI-S-CCTRL-CNPJ-CPF-SAC PIC  9(02).
              10 DCOMWAHI-S-DVCTO-PCELA-DESC   PIC  X(10).
              10 DCOMWAHI-S-VPRINC-PCELA-DESC  PIC  9(15)V9(02).
              10 DCOMWAHI-S-VJURO-PCELA-DESC   PIC  9(15)V9(02).
              10 DCOMWAHI-S-VIOF-PCELA-DESC    PIC  9(15)V9(02).
              10 DCOMWAHI-S-DEFETV-BAIXA-PCELA PIC  X(10).
              10 DCOMWAHI-S-VEFETV-BAIXA-PCELA PIC  9(15)V9(02).
              10 DCOMWAHI-S-VPRINC-PCELA-CALC  PIC  9(15)V9(02).
              10 DCOMWAHI-S-TIPO-PAGADOR       PIC  X(01).
              10 DCOMWAHI-S-IRSUMO-SIT-DESC    PIC  X(15).