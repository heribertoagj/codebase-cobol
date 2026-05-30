      ******************************************************************
      * SISTEMA : DCOM - I#DCOMRF                                      *
      * TAMANHO : 0639 BYTES                                           *
      * ARQUIVO : TITULO DESCONTO COMERCIAL ATUALIZADO COM DADOS DA    *
      *           COBRANCA (DCOMB0F1)                                  *
      ******************************************************************
       01  RF-REGISTRO.
  1  3     05 RF-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
  4  5     05 RF-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
  9  3     05 RF-NTITLO-DESC-COML     PIC S9(5)V USAGE COMP-3.
 12  2     05 RF-CBCO-PRODT-COBR      PIC S9(3)V USAGE COMP-3.
 14  2     05 RF-CIDTFD-PRODT-COBR    PIC S9(2)V USAGE COMP-3.
 16 05     05 RF-CNEGOC-COBR          PIC S9(18)V USAGE COMP-3.
 26 05     05 RF-CTITLO-COBR-BCO      PIC S9(18)V USAGE COMP-3.
 36  2     05 RF-CNRO-SEQ-TITLO       PIC S9(3)V USAGE COMP-3.
 38  6     05 RF-NNOTA-PROMS-RURAL    PIC S9(11)V USAGE COMP-3.
 44  3     05 RF-CINDCD-NOTA-PROMS    PIC X(3).
 47  2     05 RF-CTPO-PROTR-RURAL     PIC S9(2)V USAGE COMP-3.
 49  2     05 RF-CPRODT-RURAL         PIC S9(3)V USAGE COMP-3.
 51  2     05 RF-QPRODT-RURAL         PIC S9(3)V USAGE COMP-3.
 53  2     05 RF-CUND-MEDD            PIC S9(3)V USAGE COMP-3.
 55  1     05 RF-CINDCD-PGDOR-JURO    PIC X(1).
 56 05     05 RF-DEMIS-TITLO          PIC X(10).
 66 05     05 RF-DVCTO-TITLO-DESC     PIC X(10).
 76  9     05 RF-VTITLO-DESC-COML     PIC S9(15)V9(2) USAGE COMP-3.
 85  5     05 RF-CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
 90  3     05 RF-CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.
 93  2     05 RF-CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
 95 60     05 RF-IPSSOA-SACDO         PIC X(60).
155 15     05 RF-CSEU-NRO-TITLO       PIC X(15).
170  2     05 RF-CESPCE-TITLO-COBR    PIC S9(3)V USAGE COMP-3.
172  1     05 RF-CINDCD-INSTR-DESC    PIC S9(1)V USAGE COMP-3.
173  3     05 RF-CCEP                 PIC S9(5)V USAGE COMP-3.
176  2     05 RF-CCEP-COMPL           PIC S9(3)V USAGE COMP-3.
178 40     05 RF-ELOGDR               PIC X(40).
218 40     05 RF-IBAIRO               PIC X(40).
258 30     05 RF-IMUN                 PIC X(30).
288  2     05 RF-CSGL-UF              PIC X(2).
290  2     05 RF-CSIT-DESC-COML       PIC S9(3)V USAGE COMP-3.
292 26     05 RF-HSIT-DESC-COML       PIC X(26).
318  9     05 RF-VABTMT-TITLO-DESC    PIC S9(15)V9(2) USAGE COMP-3.
327  9     05 RF-VORPAG-RCBVL         PIC S9(15)V9(2) USAGE COMP-3.
336  3     05 RF-NPCELA-DESC-COML     PIC S9(5)V USAGE COMP-3.
339 26     05 RF-HULT-ATULZ           PIC X(26).
365  5     05 RF-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
370  8     05 RF-CTERM                PIC X(8).
378 05     05 RF-DINCL-REG            PIC X(10).
388  2     05 RF-CORIGE-REC           PIC S9(3)V USAGE COMP-3.
390  2     05 RF-CBCO-DEPOS           PIC S9(3)V USAGE COMP-3.
392  3     05 RF-CAG-BCRIA-DEPOS      PIC S9(5)V USAGE COMP-3.
395 20     05 RF-RANLSE-CREDT-CLI     PIC X(20).
415 05     05 RF-DSOLTC-ANLSE-CREDT   PIC X(10).
425  2     05 RF-CSIT-ANLSE-CREDT     PIC S9(3)V USAGE COMP-3.
427  1     05 RF-CINDCD-ISENC-ANLSE   PIC X(1).
428  1     05 RF-CINDCD-PROTE-TITLO   PIC X(1).
429  2     05 RF-QDIA-PROTE-TITLO     PIC S9(3)V USAGE COMP-3.
431  2     05 RF-CBCO-DEB-AUTOM       PIC S9(3)V USAGE COMP-3.
433  3     05 RF-CAG-DEB-AUTOM        PIC S9(5)V USAGE COMP-3.
436  7     05 RF-CCTA-DEB-AUTOM       PIC S9(13)V USAGE COMP-3.
443  2     05 RF-CIDTFD-PRODT-ORIGN   PIC S9(2)V USAGE COMP-3.
      ***
      ***  DADOS DO ARQUIVO DE PAGAMENTO DA COBRANCA PARA DESCONTO   ***
      ***
445  2     05 RF-VAGO1                 PIC X(02).
447  3     05 RF-SUBC-DESC             PIC 9(05)   COMP-3.
450  2     05 RF-TIPO-SERV-DESC        PIC 9(03)   COMP-3.
452  1     05 RF-VAGO2                 PIC X(01).
453  4     05 RF-SEQ-DESC              PIC 9(07)   COMP-3.
457  5     05 RF-AGE-COBR-DESC         PIC 9(05).
462  4     05 RF-LOTE-DESC             PIC 9(04).
466  5     05 RF-OPER-DESC             PIC 9(05).
471  5     05 RF-RAZAO-TITLO           PIC 9(05).
476  3     05 RF-CBCO                  PIC 9(03).
479  5     05 RF-CAG-BCRIA             PIC 9(05).
484 13     05 RF-CCTA-CORR-DESC        PIC 9(13).
497  2     05 RF-DIG-CTA-CORR-DESC     PIC X(02).
499  1     05 RF-VAGO3                 PIC X(01).
500  3     05 RF-PRODT-COBR            PIC 9(03).
503  3     05 RF-CART-DESC             PIC X(03).
506 18     05 RF-N-NRO-DESC            PIC 9(18).
524  2     05 RF-DIG-N-NRO-DESC        PIC X(02).
526  8     05 RF-DATA-DESC.
              10  RF-DIA-DESC          PIC 9(02).
              10  RF-MES-DESC          PIC 9(02).
              10  RF-ANO-DESC          PIC 9(04).
534 15     05 RF-VAGO4                 PIC X(15).
549  8     05 RF-VLR-COBR-DESC         PIC 9(13)V99 COMP-3.
557  1     05 RF-VAGO5                 PIC X(01).
558        05 RF-TP-BX-DESC            PIC X(01).
559        05 RF-NRO-SEQ-DESC          PIC 9(05)   COMP-3.
562        05 RF-COD-BCO-DESC          PIC 9(03).
565        05 RF-VAGO6.
              10  RF-FILLER            PIC X(12).
              10  RF-COD-ROTINA        PIC 9(01).
578  8     05 RF-DATA-MOVTO-DESC       PIC 9(08).
586  9     05 RF-VAGO7                 PIC X(09).
      ***
      ***  DADOS PARA MOVIMENTACAO                                   ***
      ***
595  9     05 RF-VLR-DIFERENCA          PIC S9(15)V9(2) USAGE COMP-3.
604  1     05 RF-TITULOS-DUPLICADOS     PIC X(01).
      ***     S - PAGAMENTOS JA PAGOS - DUPLICADOS
      ***     S - PAGAMENTOS BAIXADOS POR PAGAMENTO NA COBRANCA
      **                                                              **
      **----- DADOS PARA REPOSICAO NO CONE, CONVENIO E CONTRATO    ---**
      **                                                              **
605  5     05 RF-CCONTR-CONVE-DESC    PIC S9(9)V USAGE COMP-3.
610  2     05 RF-CVRSAO-CONTR-CONVE   PIC S9(3)V USAGE COMP-3.
612  2     05 RF-CELMTO-DESC-COML     PIC S9(3)V USAGE COMP-3.
614  3     05 RF-DANO-BASE            PIC S9(4)V USAGE COMP-3.
617  3     05 RF-CDEPDC               PIC S9(5)V USAGE COMP-3.
620  5     05 RF-CSEQ-STUDO           PIC S9(9)V USAGE COMP-3.
625  5     05 RF-CCART-CONE           PIC X(5).
630  5     05 RF-CCNPJ-CPF-OPER       PIC S9(9)V USAGE COMP-3.
635  3     05 RF-CFLIAL-CNPJ-OPER     PIC S9(5)V USAGE COMP-3.
638  2     05 RF-CCTRL-CNPJ-OPER      PIC S9(2)V USAGE COMP-3.
