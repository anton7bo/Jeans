// ************************************************************************ //
// The types declared in this file were generated from data read from the
// WSDL File described below:
// WSDL     : https://core.heineken.com/JDGS5/DRPService.asmx?WSDL
//  >Import : https://core.heineken.com/JDGS5/DRPService.asmx?WSDL:0
//  >Import : https://core.heineken.com/JDGS5/DRPService.asmx?WSDL:1
// Encoding : utf-8
// Codegen  : [wfUseSerializerClassForAttrs,wfGenTrueGUIDs]
// Version  : 1.0
// (17.08.2020 17:03:46 - * $Rev: 3108 $)
// ************************************************************************ //

unit DRPService5;

interface

uses InvokeRegistry, SOAPHTTPClient, Types, XSBuiltIns;

const
  AS_UNBOUNDED = false;

type

  // ************************************************************************ //
  // The following types, referred to in the WSDL document are not being represented
  // in this file. They are either aliases[@] of other types represented or were referred
  // to but never[!] declared in the document. The types from the latter category
  // typically map to predefined/known XML or Borland types; however, they could also 
  // indicate incorrect WSDL documents that failed to declare or import a schema type.
  // ************************************************************************ //
  // !:int             - "http://www.w3.org/2001/XMLSchema"
  // !:double          - "http://www.w3.org/2001/XMLSchema"
  // !:string          - "http://www.w3.org/2001/XMLSchema"
  // !:dateTime        - "http://www.w3.org/2001/XMLSchema"
  // !:decimal         - "http://www.w3.org/2001/XMLSchema"
  // !:boolean         - "http://www.w3.org/2001/XMLSchema"

  DistributorDebet     = class;                 { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  UnpaidDebetItem      = class;                 { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  DeliveryDays         = class;                 { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  SalePointBalanceKeg  = class;                 { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  Invoice              = class;                 { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  OrderDelivery        = class;                 { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  DeliveryPosition     = class;                 { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  AppliedPromoAction   = class;                 { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  DistributorDelivery  = class;                 { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  DistributorSalePointDelivery = class;         { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  PromoActionIdentity  = class;                 { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  PromoAction          = class;                 { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  SkuDiscount          = class;                 { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  PromoActionBalance   = class;                 { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  Balance              = class;                 { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  PromoBalance         = class;                 { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  BatchBalance         = class;                 { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  WarehouseBalanceKeg  = class;                 { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  ReturnItem           = class;                 { "http://jeans.heineken.com/DISTR/"[GblCplx] }

  ArrayOfDistributorDebet = array of DistributorDebet;   { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  ArrayOfUnpaidDebetItem = array of UnpaidDebetItem;   { "http://jeans.heineken.com/DISTR/"[GblCplx] }


  // ************************************************************************ //
  // XML       : DistributorDebet, global, <complexType>
  // Namespace : http://jeans.heineken.com/DISTR/
  // ************************************************************************ //
  DistributorDebet = class(TRemotable)
  private
    FContragentID: Integer;
    FDebetSum: Double;
    FDebetLimit: Double;
    FUnpaidDebets: ArrayOfUnpaidDebetItem;
  public
    destructor Destroy; override;
  published
    property ContragentID: Integer read FContragentID write FContragentID;
    property DebetSum: Double read FDebetSum write FDebetSum;
    property DebetLimit: Double read FDebetLimit write FDebetLimit;
    property UnpaidDebets: ArrayOfUnpaidDebetItem read FUnpaidDebets write FUnpaidDebets;
  end;



  // ************************************************************************ //
  // XML       : UnpaidDebetItem, global, <complexType>
  // Namespace : http://jeans.heineken.com/DISTR/
  // ************************************************************************ //
  UnpaidDebetItem = class(TRemotable)
  private
    FExtInvoiceNum: WideString;
    FExtInvoiceDate: TXSDateTime;
    FDebetSum: TXSDecimal;
  public
    destructor Destroy; override;
  published
    property ExtInvoiceNum: WideString read FExtInvoiceNum write FExtInvoiceNum;
    property ExtInvoiceDate: TXSDateTime read FExtInvoiceDate write FExtInvoiceDate;
    property DebetSum: TXSDecimal read FDebetSum write FDebetSum;
  end;

  ArrayOfDeliveryDays = array of DeliveryDays;   { "http://jeans.heineken.com/DISTR/"[GblCplx] }


  // ************************************************************************ //
  // XML       : DeliveryDays, global, <complexType>
  // Namespace : http://jeans.heineken.com/DISTR/
  // ************************************************************************ //
  DeliveryDays = class(TRemotable)
  private
    FSalePointID: Integer;
    FDays: WideString;
  published
    property SalePointID: Integer read FSalePointID write FSalePointID;
    property Days: WideString read FDays write FDays;
  end;

  ArrayOfSalePointBalanceKeg = array of SalePointBalanceKeg;   { "http://jeans.heineken.com/DISTR/"[GblCplx] }


  // ************************************************************************ //
  // XML       : SalePointBalanceKeg, global, <complexType>
  // Namespace : http://jeans.heineken.com/DISTR/
  // ************************************************************************ //
  SalePointBalanceKeg = class(TRemotable)
  private
    FSalePointID: Integer;
    FSkuID: Integer;
    FAmount: Integer;
    FDate: TXSDateTime;
  public
    destructor Destroy; override;
  published
    property SalePointID: Integer read FSalePointID write FSalePointID;
    property SkuID: Integer read FSkuID write FSkuID;
    property Amount: Integer read FAmount write FAmount;
    property Date: TXSDateTime read FDate write FDate;
  end;

  ArrayOfInvoice = array of Invoice;            { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  ArrayOfOrderDelivery = array of OrderDelivery;   { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  ArrayOfDistributorDelivery = array of DistributorDelivery;   { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  ArrayOfDistributorSalePointDelivery = array of DistributorSalePointDelivery;   { "http://jeans.heineken.com/DISTR/"[GblCplx] }


  // ************************************************************************ //
  // XML       : Invoice, global, <complexType>
  // Namespace : http://jeans.heineken.com/DISTR/
  // ************************************************************************ //
  Invoice = class(TRemotable)
  private
    FInvoiceNum: WideString;
    FInvoiceDate: WideString;
    FOrderDeliveries: ArrayOfOrderDelivery;
    FDistributorDeliveries: ArrayOfDistributorDelivery;
    FDistributorSalePointDeliveries: ArrayOfDistributorSalePointDelivery;
  public
    destructor Destroy; override;
  published
    property InvoiceNum: WideString read FInvoiceNum write FInvoiceNum;
    property InvoiceDate: WideString read FInvoiceDate write FInvoiceDate;
    property OrderDeliveries: ArrayOfOrderDelivery read FOrderDeliveries write FOrderDeliveries;
    property DistributorDeliveries: ArrayOfDistributorDelivery read FDistributorDeliveries write FDistributorDeliveries;
    property DistributorSalePointDeliveries: ArrayOfDistributorSalePointDelivery read FDistributorSalePointDeliveries write FDistributorSalePointDeliveries;
  end;

  ArrayOfDeliveryPosition = array of DeliveryPosition;   { "http://jeans.heineken.com/DISTR/"[GblCplx] }


  // ************************************************************************ //
  // XML       : OrderDelivery, global, <complexType>
  // Namespace : http://jeans.heineken.com/DISTR/
  // ************************************************************************ //
  OrderDelivery = class(TRemotable)
  private
    FDeliveryPositions: ArrayOfDeliveryPosition;
    FOrderID: WideString;
  public
    destructor Destroy; override;
  published
    property DeliveryPositions: ArrayOfDeliveryPosition read FDeliveryPositions write FDeliveryPositions;
    property OrderID: WideString read FOrderID write FOrderID;
  end;

  ArrayOfAppliedPromoAction = array of AppliedPromoAction;   { "http://jeans.heineken.com/DISTR/"[GblCplx] }


  // ************************************************************************ //
  // XML       : DeliveryPosition, global, <complexType>
  // Namespace : http://jeans.heineken.com/DISTR/
  // ************************************************************************ //
  DeliveryPosition = class(TRemotable)
  private
    FSkuID: WideString;
    FVolume: WideString;
    FRubles: WideString;
    FBasePrice: WideString;
    FCount: WideString;
    FComments: WideString;
    FTPRDiscount: WideString;
    FOtherDiscount: WideString;
    FAppliedPromoActions: ArrayOfAppliedPromoAction;
  public
    destructor Destroy; override;
  published
    property SkuID: WideString read FSkuID write FSkuID;
    property Volume: WideString read FVolume write FVolume;
    property Rubles: WideString read FRubles write FRubles;
    property BasePrice: WideString read FBasePrice write FBasePrice;
    property Count: WideString read FCount write FCount;
    property Comments: WideString read FComments write FComments;
    property TPRDiscount: WideString read FTPRDiscount write FTPRDiscount;
    property OtherDiscount: WideString read FOtherDiscount write FOtherDiscount;
    property AppliedPromoActions: ArrayOfAppliedPromoAction read FAppliedPromoActions write FAppliedPromoActions;
  end;



  // ************************************************************************ //
  // XML       : AppliedPromoAction, global, <complexType>
  // Namespace : http://jeans.heineken.com/DISTR/
  // ************************************************************************ //
  AppliedPromoAction = class(TRemotable)
  private
    FPromoId: WideString;
    FExtPromoID: WideString;
    FExtPromoName: WideString;
    FPricePerUnit: WideString;
    FDiscountPerUnit: WideString;
  published
    property PromoId: WideString read FPromoId write FPromoId;
    property ExtPromoID: WideString read FExtPromoID write FExtPromoID;
    property ExtPromoName: WideString read FExtPromoName write FExtPromoName;
    property PricePerUnit: WideString read FPricePerUnit write FPricePerUnit;
    property DiscountPerUnit: WideString read FDiscountPerUnit write FDiscountPerUnit;
  end;



  // ************************************************************************ //
  // XML       : DistributorDelivery, global, <complexType>
  // Namespace : http://jeans.heineken.com/DISTR/
  // ************************************************************************ //
  DistributorDelivery = class(TRemotable)
  private
    FDeliveryPositions: ArrayOfDeliveryPosition;
    FDistributorOrderID: WideString;
    FSalePointID: WideString;
  public
    destructor Destroy; override;
  published
    property DeliveryPositions: ArrayOfDeliveryPosition read FDeliveryPositions write FDeliveryPositions;
    property DistributorOrderID: WideString read FDistributorOrderID write FDistributorOrderID;
    property SalePointID: WideString read FSalePointID write FSalePointID;
  end;



  // ************************************************************************ //
  // XML       : DistributorSalePointDelivery, global, <complexType>
  // Namespace : http://jeans.heineken.com/DISTR/
  // ************************************************************************ //
  DistributorSalePointDelivery = class(TRemotable)
  private
    FDeliveryPositions: ArrayOfDeliveryPosition;
    FDistributorOrderID: WideString;
    FDistributorSalePointID: WideString;
    FSalePointName: WideString;
    FSalePointAddress: WideString;
    FWarehouseID: WideString;
  public
    destructor Destroy; override;
  published
    property DeliveryPositions: ArrayOfDeliveryPosition read FDeliveryPositions write FDeliveryPositions;
    property DistributorOrderID: WideString read FDistributorOrderID write FDistributorOrderID;
    property DistributorSalePointID: WideString read FDistributorSalePointID write FDistributorSalePointID;
    property SalePointName: WideString read FSalePointName write FSalePointName;
    property SalePointAddress: WideString read FSalePointAddress write FSalePointAddress;
    property WarehouseID: WideString read FWarehouseID write FWarehouseID;
  end;

  ArrayOfPromoAction = array of PromoAction;    { "http://jeans.heineken.com/DISTR/"[GblCplx] }


  // ************************************************************************ //
  // XML       : PromoActionIdentity, global, <complexType>
  // Namespace : http://jeans.heineken.com/DISTR/
  // ************************************************************************ //
  PromoActionIdentity = class(TRemotable)
  private
    FPromoID: Integer;
    FExtPromoID: WideString;
  published
    property PromoID: Integer read FPromoID write FPromoID;
    property ExtPromoID: WideString read FExtPromoID write FExtPromoID;
  end;

  ArrayOfInt = array of Integer;                { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  ArrayOfSkuDiscount = array of SkuDiscount;    { "http://jeans.heineken.com/DISTR/"[GblCplx] }


  // ************************************************************************ //
  // XML       : PromoAction, global, <complexType>
  // Namespace : http://jeans.heineken.com/DISTR/
  // ************************************************************************ //
  PromoAction = class(PromoActionIdentity)
  private
    FExtPromoName: WideString;
    FStartedAt: WideString;
    FFinishedAt: WideString;
    FComment: WideString;
    FDiscountCondition: WideString;
    FDiscountType: WideString;
    FPromoType: WideString;
    FDiscountSource: WideString;
    FDiscountLimitRub: WideString;
    FTargets: ArrayOfInt;
    FSkus: ArrayOfSkuDiscount;
  public
    destructor Destroy; override;
  published
    property ExtPromoName: WideString read FExtPromoName write FExtPromoName;
    property StartedAt: WideString read FStartedAt write FStartedAt;
    property FinishedAt: WideString read FFinishedAt write FFinishedAt;
    property Comment: WideString read FComment write FComment;
    property DiscountCondition: WideString read FDiscountCondition write FDiscountCondition;
    property DiscountType: WideString read FDiscountType write FDiscountType;
    property PromoType: WideString read FPromoType write FPromoType;
    property DiscountSource: WideString read FDiscountSource write FDiscountSource;
    property DiscountLimitRub: WideString read FDiscountLimitRub write FDiscountLimitRub;
    property Targets: ArrayOfInt read FTargets write FTargets;
    property Skus: ArrayOfSkuDiscount read FSkus write FSkus;
  end;



  // ************************************************************************ //
  // XML       : SkuDiscount, global, <complexType>
  // Namespace : http://jeans.heineken.com/DISTR/
  // ************************************************************************ //
  SkuDiscount = class(TRemotable)
  private
    FSkuID: WideString;
    FDiscountValue: WideString;
    FDiscountLimit: WideString;
  published
    property SkuID: WideString read FSkuID write FSkuID;
    property DiscountValue: WideString read FDiscountValue write FDiscountValue;
    property DiscountLimit: WideString read FDiscountLimit write FDiscountLimit;
  end;

  ArrayOfPromoActionBalance = array of PromoActionBalance;   { "http://jeans.heineken.com/DISTR/"[GblCplx] }


  // ************************************************************************ //
  // XML       : PromoActionBalance, global, <complexType>
  // Namespace : http://jeans.heineken.com/DISTR/
  // ************************************************************************ //
  PromoActionBalance = class(PromoActionIdentity)
  private
    FDiscountBalanceRub: Integer;
  published
    property DiscountBalanceRub: Integer read FDiscountBalanceRub write FDiscountBalanceRub;
  end;

  ArrayOfPromoActionIdentity = array of PromoActionIdentity;   { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  ArrayOfBalance = array of Balance;            { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  ArrayOfPromoBalance = array of PromoBalance;   { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  ArrayOfBatchBalance = array of BatchBalance;   { "http://jeans.heineken.com/DISTR/"[GblCplx] }


  // ************************************************************************ //
  // XML       : Balance, global, <complexType>
  // Namespace : http://jeans.heineken.com/DISTR/
  // ************************************************************************ //
  Balance = class(TRemotable)
  private
    FSkuID: Integer;
    FWarehouseID: Integer;
    FAmount: Integer;
    FRegularAvailableAmount: Integer;
    FPromoBalances: ArrayOfPromoBalance;
    FBatchBalances: ArrayOfBatchBalance;
    FDate: TXSDateTime;
  public
    destructor Destroy; override;
  published
    property SkuID: Integer read FSkuID write FSkuID;
    property WarehouseID: Integer read FWarehouseID write FWarehouseID;
    property Amount: Integer read FAmount write FAmount;
    property RegularAvailableAmount: Integer read FRegularAvailableAmount write FRegularAvailableAmount;
    property PromoBalances: ArrayOfPromoBalance read FPromoBalances write FPromoBalances;
    property BatchBalances: ArrayOfBatchBalance read FBatchBalances write FBatchBalances;
    property Date: TXSDateTime read FDate write FDate;
  end;



  // ************************************************************************ //
  // XML       : PromoBalance, global, <complexType>
  // Namespace : http://jeans.heineken.com/DISTR/
  // ************************************************************************ //
  PromoBalance = class(TRemotable)
  private
    FPromoID: Integer;
    FExtPromoID: WideString;
    FPromoAvailableAmount: Integer;
  published
    property PromoID: Integer read FPromoID write FPromoID;
    property ExtPromoID: WideString read FExtPromoID write FExtPromoID;
    property PromoAvailableAmount: Integer read FPromoAvailableAmount write FPromoAvailableAmount;
  end;



  // ************************************************************************ //
  // XML       : BatchBalance, global, <complexType>
  // Namespace : http://jeans.heineken.com/DISTR/
  // ************************************************************************ //
  BatchBalance = class(TRemotable)
  private
    FBatchNum: WideString;
    FInvoiceNum: WideString;
    FBatchAmount: Integer;
    FProductionDate: TXSDateTime;
    FExpirationDate: TXSDateTime;
  public
    destructor Destroy; override;
  published
    property BatchNum: WideString read FBatchNum write FBatchNum;
    property InvoiceNum: WideString read FInvoiceNum write FInvoiceNum;
    property BatchAmount: Integer read FBatchAmount write FBatchAmount;
    property ProductionDate: TXSDateTime read FProductionDate write FProductionDate;
    property ExpirationDate: TXSDateTime read FExpirationDate write FExpirationDate;
  end;

  ArrayOfWarehouseBalanceKeg = array of WarehouseBalanceKeg;   { "http://jeans.heineken.com/DISTR/"[GblCplx] }


  // ************************************************************************ //
  // XML       : WarehouseBalanceKeg, global, <complexType>
  // Namespace : http://jeans.heineken.com/DISTR/
  // ************************************************************************ //
  WarehouseBalanceKeg = class(TRemotable)
  private
    FWarehouseID: Integer;
    FSkuID: Integer;
    FAmount: Integer;
    FDate: TXSDateTime;
  public
    destructor Destroy; override;
  published
    property WarehouseID: Integer read FWarehouseID write FWarehouseID;
    property SkuID: Integer read FSkuID write FSkuID;
    property Amount: Integer read FAmount write FAmount;
    property Date: TXSDateTime read FDate write FDate;
  end;

  ArrayOfReturnItem = array of ReturnItem;      { "http://jeans.heineken.com/DISTR/"[GblCplx] }
  guid            =  type WideString;      { "http://microsoft.com/wsdl/types/"[GblSmpl] }


  // ************************************************************************ //
  // XML       : ReturnItem, global, <complexType>
  // Namespace : http://jeans.heineken.com/DISTR/
  // ************************************************************************ //
  ReturnItem = class(TRemotable)
  private
    FReturnID: guid;
    FStatus: WideString;
    FComments: WideString;
  published
    property ReturnID: guid read FReturnID write FReturnID;
    property Status: WideString read FStatus write FStatus;
    property Comments: WideString read FComments write FComments;
  end;

  GetOrdersResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }
  GetPromoActionsResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }
  GetSalePointsResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }
  GetKegDiscrepancyResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }
  GetReturnsResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }
  SendAllContragentDebetResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }
  SendDeliveryDaysResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }
  SendSalepointBalanceKegResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }
  SendSellOutResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }
  SendPromoActionsResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }
  SendBalancePromoActionsResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }
  SendChangesPromoActionsResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }
  DeletePromoActionsResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }
  SendWarehouseBalanceResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }
  SendWarehouseBalanceKegResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }
  SendReturnStatusResult =  type WideString;      { "http://jeans.heineken.com/DISTR/"[Cplx] }

  // ************************************************************************ //
  // Namespace : http://jeans.heineken.com/DISTR/
  // soapAction: http://jeans.heineken.com/DISTR/%operationName%
  // transport : http://schemas.xmlsoap.org/soap/http
  // style     : document
  // binding   : DRPServiceSoap
  // service   : DRPService
  // port      : DRPServiceSoap
  // URL       : https://core.heineken.com/JDGS5/DRPService.asmx
  // ************************************************************************ //
  DRPServiceSoap5 = interface(IInvokable)
  ['{9591C063-0803-4EA9-BF8F-9C323A37623F}']
    function  GetVersion: WideString; stdcall;
    function  GetOrders(const _login: WideString; const _pass: WideString; const _date: TXSDateTime): GetOrdersResult; stdcall;
    function  GetPromoActions(const _login: WideString; const _pass: WideString; const _from: TXSDateTime): GetPromoActionsResult; stdcall;
    function  GetSalePoints(const _login: WideString; const _pass: WideString; const _from: TXSDateTime): GetSalePointsResult; stdcall;
    function  GetKegDiscrepancy(const _login: WideString; const _pass: WideString; const date: TXSDateTime; const withDiscrepancy: Boolean): GetKegDiscrepancyResult; stdcall;
    function  GetReturns(const _login: WideString; const _pass: WideString; const date: TXSDateTime; const returnType: WideString): GetReturnsResult; stdcall;
    function  SendAllContragentDebet(const _login: WideString; const _pass: WideString; const _contragentDebets: ArrayOfDistributorDebet): SendAllContragentDebetResult; stdcall;
    function  SendDeliveryDays(const _login: WideString; const _pass: WideString; const items: ArrayOfDeliveryDays): SendDeliveryDaysResult; stdcall;
    function  SendSalepointBalanceKeg(const _login: WideString; const _pass: WideString; const items: ArrayOfSalePointBalanceKeg): SendSalepointBalanceKegResult; stdcall;
    function  SendSellOut(const _login: WideString; const _pass: WideString; const _invoices: ArrayOfInvoice): SendSellOutResult; stdcall;
    function  SendPromoActions(const _login: WideString; const _pass: WideString; const _promoActions: ArrayOfPromoAction): SendPromoActionsResult; stdcall;
    function  SendBalancePromoActions(const _login: WideString; const _pass: WideString; const _promoActionBalances: ArrayOfPromoActionBalance): SendBalancePromoActionsResult; stdcall;
    function  SendChangesPromoActions(const _login: WideString; const _pass: WideString; const _promoActions: ArrayOfPromoAction): SendChangesPromoActionsResult; stdcall;
    function  DeletePromoActions(const _login: WideString; const _pass: WideString; const _promoActions: ArrayOfPromoActionIdentity): DeletePromoActionsResult; stdcall;
    function  SendWarehouseBalance(const _login: WideString; const _pass: WideString; const items: ArrayOfBalance): SendWarehouseBalanceResult; stdcall;
    function  SendWarehouseBalanceKeg(const _login: WideString; const _pass: WideString; const items: ArrayOfWarehouseBalanceKeg): SendWarehouseBalanceKegResult; stdcall;
    function  SendReturnStatus(const _login: WideString; const _pass: WideString; const items: ArrayOfReturnItem): SendReturnStatusResult; stdcall;
  end;

function GetDRPServiceSoap(UseWSDL: Boolean=System.False; Addr: string=''; HTTPRIO: THTTPRIO = nil): DRPServiceSoap5;


implementation
  uses SysUtils;

function GetDRPServiceSoap(UseWSDL: Boolean; Addr: string; HTTPRIO: THTTPRIO): DRPServiceSoap5;
const
  defWSDL = 'https://core.heineken.com/JDGS5/DRPService.asmx?WSDL';
  defURL  = 'https://core.heineken.com/JDGS5/DRPService.asmx';
  defSvc  = 'DRPService';
  defPrt  = 'DRPServiceSoap';
var
  RIO: THTTPRIO;
begin
  Result := nil;
  if (Addr = '') then
  begin
    if UseWSDL then
      Addr := defWSDL
    else
      Addr := defURL;
  end;
  if HTTPRIO = nil then
    RIO := THTTPRIO.Create(nil)
  else
    RIO := HTTPRIO;
  try
    Result := (RIO as DRPServiceSoap5);
    if UseWSDL then
    begin
      RIO.WSDLLocation := Addr;
      RIO.Service := defSvc;
      RIO.Port := defPrt;
    end else
      RIO.URL := Addr;
  finally
    if (Result = nil) and (HTTPRIO = nil) then
      RIO.Free;
  end;
end;


destructor DistributorDebet.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FUnpaidDebets)-1 do
    FreeAndNil(FUnpaidDebets[I]);
  SetLength(FUnpaidDebets, 0);
  inherited Destroy;
end;

destructor UnpaidDebetItem.Destroy;
begin
  FreeAndNil(FExtInvoiceDate);
  FreeAndNil(FDebetSum);
  inherited Destroy;
end;

destructor SalePointBalanceKeg.Destroy;
begin
  FreeAndNil(FDate);
  inherited Destroy;
end;

destructor Invoice.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FOrderDeliveries)-1 do
    FreeAndNil(FOrderDeliveries[I]);
  SetLength(FOrderDeliveries, 0);
  for I := 0 to Length(FDistributorDeliveries)-1 do
    FreeAndNil(FDistributorDeliveries[I]);
  SetLength(FDistributorDeliveries, 0);
  for I := 0 to Length(FDistributorSalePointDeliveries)-1 do
    FreeAndNil(FDistributorSalePointDeliveries[I]);
  SetLength(FDistributorSalePointDeliveries, 0);
  inherited Destroy;
end;

destructor OrderDelivery.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FDeliveryPositions)-1 do
    FreeAndNil(FDeliveryPositions[I]);
  SetLength(FDeliveryPositions, 0);
  inherited Destroy;
end;

destructor DeliveryPosition.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FAppliedPromoActions)-1 do
    FreeAndNil(FAppliedPromoActions[I]);
  SetLength(FAppliedPromoActions, 0);
  inherited Destroy;
end;

destructor DistributorDelivery.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FDeliveryPositions)-1 do
    FreeAndNil(FDeliveryPositions[I]);
  SetLength(FDeliveryPositions, 0);
  inherited Destroy;
end;

destructor DistributorSalePointDelivery.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FDeliveryPositions)-1 do
    FreeAndNil(FDeliveryPositions[I]);
  SetLength(FDeliveryPositions, 0);
  inherited Destroy;
end;

destructor PromoAction.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FSkus)-1 do
    FreeAndNil(FSkus[I]);
  SetLength(FSkus, 0);
  inherited Destroy;
end;

destructor Balance.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FPromoBalances)-1 do
    FreeAndNil(FPromoBalances[I]);
  SetLength(FPromoBalances, 0);
  for I := 0 to Length(FBatchBalances)-1 do
    FreeAndNil(FBatchBalances[I]);
  SetLength(FBatchBalances, 0);
  FreeAndNil(FDate);
  inherited Destroy;
end;

destructor BatchBalance.Destroy;
begin
  FreeAndNil(FProductionDate);
  FreeAndNil(FExpirationDate);
  inherited Destroy;
end;

destructor WarehouseBalanceKeg.Destroy;
begin
  FreeAndNil(FDate);
  inherited Destroy;
end;

initialization
  InvRegistry.RegisterInterface(TypeInfo(DRPServiceSoap5), 'http://jeans.heineken.com/DISTR/', 'utf-8');
  InvRegistry.RegisterDefaultSOAPAction(TypeInfo(DRPServiceSoap5), 'http://jeans.heineken.com/DISTR/%operationName%');
  InvRegistry.RegisterInvokeOptions(TypeInfo(DRPServiceSoap5), ioDocument);
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfDistributorDebet), 'http://jeans.heineken.com/DISTR/', 'ArrayOfDistributorDebet');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfUnpaidDebetItem), 'http://jeans.heineken.com/DISTR/', 'ArrayOfUnpaidDebetItem');
  RemClassRegistry.RegisterXSClass(DistributorDebet, 'http://jeans.heineken.com/DISTR/', 'DistributorDebet');
  RemClassRegistry.RegisterXSClass(UnpaidDebetItem, 'http://jeans.heineken.com/DISTR/', 'UnpaidDebetItem');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfDeliveryDays), 'http://jeans.heineken.com/DISTR/', 'ArrayOfDeliveryDays');
  RemClassRegistry.RegisterXSClass(DeliveryDays, 'http://jeans.heineken.com/DISTR/', 'DeliveryDays');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfSalePointBalanceKeg), 'http://jeans.heineken.com/DISTR/', 'ArrayOfSalePointBalanceKeg');
  RemClassRegistry.RegisterXSClass(SalePointBalanceKeg, 'http://jeans.heineken.com/DISTR/', 'SalePointBalanceKeg');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfInvoice), 'http://jeans.heineken.com/DISTR/', 'ArrayOfInvoice');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfOrderDelivery), 'http://jeans.heineken.com/DISTR/', 'ArrayOfOrderDelivery');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfDistributorDelivery), 'http://jeans.heineken.com/DISTR/', 'ArrayOfDistributorDelivery');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfDistributorSalePointDelivery), 'http://jeans.heineken.com/DISTR/', 'ArrayOfDistributorSalePointDelivery');
  RemClassRegistry.RegisterXSClass(Invoice, 'http://jeans.heineken.com/DISTR/', 'Invoice');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfDeliveryPosition), 'http://jeans.heineken.com/DISTR/', 'ArrayOfDeliveryPosition');
  RemClassRegistry.RegisterXSClass(OrderDelivery, 'http://jeans.heineken.com/DISTR/', 'OrderDelivery');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfAppliedPromoAction), 'http://jeans.heineken.com/DISTR/', 'ArrayOfAppliedPromoAction');
  RemClassRegistry.RegisterXSClass(DeliveryPosition, 'http://jeans.heineken.com/DISTR/', 'DeliveryPosition');
  RemClassRegistry.RegisterXSClass(AppliedPromoAction, 'http://jeans.heineken.com/DISTR/', 'AppliedPromoAction');
  RemClassRegistry.RegisterXSClass(DistributorDelivery, 'http://jeans.heineken.com/DISTR/', 'DistributorDelivery');
  RemClassRegistry.RegisterXSClass(DistributorSalePointDelivery, 'http://jeans.heineken.com/DISTR/', 'DistributorSalePointDelivery');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfPromoAction), 'http://jeans.heineken.com/DISTR/', 'ArrayOfPromoAction');
  RemClassRegistry.RegisterXSClass(PromoActionIdentity, 'http://jeans.heineken.com/DISTR/', 'PromoActionIdentity');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfInt), 'http://jeans.heineken.com/DISTR/', 'ArrayOfInt');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfSkuDiscount), 'http://jeans.heineken.com/DISTR/', 'ArrayOfSkuDiscount');
  RemClassRegistry.RegisterXSClass(PromoAction, 'http://jeans.heineken.com/DISTR/', 'PromoAction');
  RemClassRegistry.RegisterXSClass(SkuDiscount, 'http://jeans.heineken.com/DISTR/', 'SkuDiscount');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfPromoActionBalance), 'http://jeans.heineken.com/DISTR/', 'ArrayOfPromoActionBalance');
  RemClassRegistry.RegisterXSClass(PromoActionBalance, 'http://jeans.heineken.com/DISTR/', 'PromoActionBalance');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfPromoActionIdentity), 'http://jeans.heineken.com/DISTR/', 'ArrayOfPromoActionIdentity');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfBalance), 'http://jeans.heineken.com/DISTR/', 'ArrayOfBalance');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfPromoBalance), 'http://jeans.heineken.com/DISTR/', 'ArrayOfPromoBalance');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfBatchBalance), 'http://jeans.heineken.com/DISTR/', 'ArrayOfBatchBalance');
  RemClassRegistry.RegisterXSClass(Balance, 'http://jeans.heineken.com/DISTR/', 'Balance');
  RemClassRegistry.RegisterXSClass(PromoBalance, 'http://jeans.heineken.com/DISTR/', 'PromoBalance');
  RemClassRegistry.RegisterXSClass(BatchBalance, 'http://jeans.heineken.com/DISTR/', 'BatchBalance');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfWarehouseBalanceKeg), 'http://jeans.heineken.com/DISTR/', 'ArrayOfWarehouseBalanceKeg');
  RemClassRegistry.RegisterXSClass(WarehouseBalanceKeg, 'http://jeans.heineken.com/DISTR/', 'WarehouseBalanceKeg');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfReturnItem), 'http://jeans.heineken.com/DISTR/', 'ArrayOfReturnItem');
  RemClassRegistry.RegisterXSInfo(TypeInfo(guid), 'http://microsoft.com/wsdl/types/', 'guid');
  RemClassRegistry.RegisterXSClass(ReturnItem, 'http://jeans.heineken.com/DISTR/', 'ReturnItem');
  RemClassRegistry.RegisterXSInfo(TypeInfo(GetOrdersResult), 'http://jeans.heineken.com/DISTR/', 'GetOrdersResult');
  RemClassRegistry.RegisterXSInfo(TypeInfo(GetPromoActionsResult), 'http://jeans.heineken.com/DISTR/', 'GetPromoActionsResult');
  RemClassRegistry.RegisterXSInfo(TypeInfo(GetSalePointsResult), 'http://jeans.heineken.com/DISTR/', 'GetSalePointsResult');
  RemClassRegistry.RegisterXSInfo(TypeInfo(GetKegDiscrepancyResult), 'http://jeans.heineken.com/DISTR/', 'GetKegDiscrepancyResult');
  RemClassRegistry.RegisterXSInfo(TypeInfo(GetReturnsResult), 'http://jeans.heineken.com/DISTR/', 'GetReturnsResult');
  RemClassRegistry.RegisterXSInfo(TypeInfo(SendAllContragentDebetResult), 'http://jeans.heineken.com/DISTR/', 'SendAllContragentDebetResult');
  RemClassRegistry.RegisterXSInfo(TypeInfo(SendDeliveryDaysResult), 'http://jeans.heineken.com/DISTR/', 'SendDeliveryDaysResult');
  RemClassRegistry.RegisterXSInfo(TypeInfo(SendSalepointBalanceKegResult), 'http://jeans.heineken.com/DISTR/', 'SendSalepointBalanceKegResult');
  RemClassRegistry.RegisterXSInfo(TypeInfo(SendSellOutResult), 'http://jeans.heineken.com/DISTR/', 'SendSellOutResult');
  RemClassRegistry.RegisterXSInfo(TypeInfo(SendPromoActionsResult), 'http://jeans.heineken.com/DISTR/', 'SendPromoActionsResult');
  RemClassRegistry.RegisterXSInfo(TypeInfo(SendBalancePromoActionsResult), 'http://jeans.heineken.com/DISTR/', 'SendBalancePromoActionsResult');
  RemClassRegistry.RegisterXSInfo(TypeInfo(SendChangesPromoActionsResult), 'http://jeans.heineken.com/DISTR/', 'SendChangesPromoActionsResult');
  RemClassRegistry.RegisterXSInfo(TypeInfo(DeletePromoActionsResult), 'http://jeans.heineken.com/DISTR/', 'DeletePromoActionsResult');
  RemClassRegistry.RegisterXSInfo(TypeInfo(SendWarehouseBalanceResult), 'http://jeans.heineken.com/DISTR/', 'SendWarehouseBalanceResult');
  RemClassRegistry.RegisterXSInfo(TypeInfo(SendWarehouseBalanceKegResult), 'http://jeans.heineken.com/DISTR/', 'SendWarehouseBalanceKegResult');
  RemClassRegistry.RegisterXSInfo(TypeInfo(SendReturnStatusResult), 'http://jeans.heineken.com/DISTR/', 'SendReturnStatusResult');

end.